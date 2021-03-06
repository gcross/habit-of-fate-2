{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.XML (runParserOnFile) where

import Control.Category ((>>>))
import Control.Lens (Prism',(^?),(<&>),prism')
import Control.Lens.Extras (is)
import Control.Monad ((>=>),forM_,unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Data.Text (Text,unpack)
import Flow ((|>))
import System.FilePath ((</>),isAbsolute)
import qualified Text.Parsec as Parsec
import Text.Parsec
  ( ParsecT
  , (<|>)
  , between
  , getState
  , optional
  , putState
  , sepEndBy
  , sepEndBy1
  , setSourceColumn
  , setSourceLine
  , skipMany
  , tokenPrim
  )
import qualified Text.XML.Expat.SAX as SAX
import Text.XML.Expat.SAX (SAXEvent(..),XMLParseLocation(..))

import HabitOfFate.Data.Content
import HabitOfFate.Data.Event
import HabitOfFate.Data.Gender
import HabitOfFate.Data.Narrative
import HabitOfFate.Data.Occurance
import qualified HabitOfFate.Data.Story as Story
import HabitOfFate.Data.Story
import HabitOfFate.Data.Substitutions
import HabitOfFate.Operators ((⊕),(∈))
import HabitOfFate.Substitution

import Paths_habit_of_fate

type XMLEvent = SAXEvent Text Text

_XMLDeclaration ∷ Prism' XMLEvent (Text, Maybe Text, Maybe Bool)
_XMLDeclaration = prism'
  (\(a,b,c) → XMLDeclaration a b c)
  (\case {XMLDeclaration a b c → Just (a,b,c); _ → Nothing})

_StartElement ∷ Prism' XMLEvent (Text, [(Text,Text)])
_StartElement = prism'
  (\(a,b) → StartElement a b)
  (\case {StartElement a b → Just (a,b); _ → Nothing})

_EndElement ∷ Prism' XMLEvent Text
_EndElement = prism'
  (\a → EndElement a)
  (\case {EndElement a → Just a; _ → Nothing})

_CharacterData ∷ Prism' XMLEvent Text
_CharacterData = prism'
  (\a → CharacterData a)
  (\case {CharacterData a → Just a; _ → Nothing})

_Comment ∷ Prism' XMLEvent Text
_Comment = prism'
  (\a → Comment a)
  (\case {Comment a → Just a; _ → Nothing})

type Parser = ParsecT [(XMLEvent,XMLParseLocation)] (HashSet Text) IO

sepEndByNonEmpty ∷ Parser α → Parser sep → Parser (NonEmpty α)
sepEndByNonEmpty p psep =
  sepEndBy p psep
  >>=
  \case
    (x:xs) → pure (x:|xs)
    [] → fail "at least one element required"

parseToken ∷ (XMLEvent → Maybe α) → Parser α
parseToken testToken = tokenPrim
  (fst >>> show)
  (\old_pos (_,XMLParseLocation{..}) _ → old_pos
    |> flip setSourceLine (fromIntegral xmlLineNumber)
    |> flip setSourceColumn (fromIntegral xmlColumnNumber)
  )
  (fst >>> testToken)

{-
-- Currently unused, commented out to silence the warning.

consumeToken ∷ (XMLEvent → Bool) → Parser ()
consumeToken testToken = parseToken (testToken >>> bool Nothing (Just ()))

nextToken ∷ Parser XMLEvent
nextToken = parseToken Just
-}

xmlDeclaration ∷ Parser ()
xmlDeclaration = parseToken (is _XMLDeclaration >>> bool Nothing (Just ()))

startElement ∷ Text → Parser [(Text,Text)]
startElement expected_tag = parseToken $
  (^? _StartElement)
  >=>
  \(tag,attributes) →
    if tag == expected_tag
      then Just attributes
      else Nothing

endElement ∷ Text → Parser ()
endElement expected_tag = parseToken $
  (^? _EndElement)
  >=>
  ((== expected_tag) >>> bool Nothing (Just ()))

isWhitespace ∷ Char → Bool
isWhitespace = (∈ (" \t\r\n" ∷ Text))

whitespace ∷ Parser ()
whitespace = parseToken $
  (^? _CharacterData)
  >=>
  (Text.all isWhitespace >>> bool Nothing (Just ()))

comment ∷ Parser ()
comment = parseToken (is _Comment >>> bool Nothing (Just ()))

skipComments ∷ Parser ()
skipComments = skipMany comment

skipWhitespaceAndComments ∷ Parser ()
skipWhitespaceAndComments = skipMany (whitespace <|> comment)

manyNonEmptyIgnoringSurroundingWhitespaceAndComments ∷ Parser α → Parser (NonEmpty α)
manyNonEmptyIgnoringSurroundingWhitespaceAndComments p = do
  optional skipWhitespaceAndComments
  sepEndByNonEmpty p skipWhitespaceAndComments

characterData ∷ Parser Text
characterData = parseToken $ (^? _CharacterData)

characterDataSkippingComments1 ∷ Parser Text
characterDataSkippingComments1 = mconcat <$> (optional skipComments >> sepEndBy1 characterData skipComments)

parseAttributes ∷ [Text] → [(Text,Text)] → Parser [Text]
parseAttributes expected_keys attributes = do
  let keys = map fst attributes
      unexpected_keys = keys \\ expected_keys
      missing_keys = expected_keys \\ keys
  unless (null unexpected_keys) $ fail $ "unexpected keys:" ⊕ show (keys \\ unexpected_keys)
  unless (null missing_keys   ) $ fail $ "missing keys:"    ⊕ show (keys \\ missing_keys)
  pure $ map (flip lookup attributes >>> fromJust) expected_keys

startElementWithAttributes ∷ Text → [Text] → Parser [Text]
startElementWithAttributes element_name expected_attribute_names =
  startElement element_name
  >>=
  parseAttributes expected_attribute_names

withElement ∷ Text → [Text] → ([Text] → Parser α) → Parser α
withElement element_name expected_attribute_names parseUsingAttributes = do
  result ← startElementWithAttributes element_name expected_attribute_names >>= parseUsingAttributes
  endElement element_name
  pure result

parseGender ∷ Text → Parser Gender
parseGender "male" = pure Male
parseGender "female" = pure Female
parseGender "neuter" = pure Neuter
parseGender _ = fail "unexpected gender"

parseGendered ∷ Parser Gendered
parseGendered = withElement "candidate" ["name","gender"] $ \[name,gender_string] →
  Gendered name <$> parseGender gender_string

parseSubstitute ∷ Parser Substitute
parseSubstitute = withElement "substitute" ["placeholder"] $ \[placeholder] → do
  Substitute placeholder <$> manyNonEmptyIgnoringSurroundingWhitespaceAndComments parseGendered

parseFame ∷ Parser Content
parseFame = withElement "fame" [] $ \[] → parseContent

parseTextSubstitutions ∷ Text → Parser Substitutions
parseTextSubstitutions text = do
  substitutions ← text |> parseSubstitutions |> either (show >>> fail) pure
  known_placeholders ← getState
  forM_ (extractPlaceholders substitutions) $ \placeholder →
    unless (HashSet.member placeholder known_placeholders) $
      fail $ "no given substitution for placeholder " ⊕ unpack placeholder
  pure substitutions

parseContentChunk ∷ Parser ContentChunk
parseContentChunk =
      (Unformatted <$> (characterDataSkippingComments1 >>= parseTextSubstitutions))
  <|> (Bold <$> withElement "b" [] (const parseContent))

parseContent ∷ Parser Content
parseContent = do
  optional skipComments
  sepEndBy1 parseContentChunk skipComments <&> Content

parseParagraph ∷ Parser Content
parseParagraph = withElement "p" [] $ \[] → parseContent

parseBodyContent ∷ Parser BodyContent
parseBodyContent = do
  optional skipWhitespaceAndComments
  sepEndBy1 parseParagraph skipWhitespaceAndComments <&> BodyContent

parseNarrative ∷ Parser Narrative
parseNarrative = withElement "narrative" ["title"] $ \[title] →
  Narrative <$> parseTextSubstitutions title <*> parseBodyContent

parseEvent ∷ Parser Event
parseEvent = withElement "event" ["title","question"] $ \[event_title,event_question] → do
  common_narrative ← Narrative <$> parseTextSubstitutions event_title <*> parseBodyContent
  common_question ← parseTextSubstitutions event_question
  skipWhitespaceAndComments
  (success_choice,success_narrative) ←
    withElement "success" ["choice","title"] $ \[choice,title] → (,)
      <$> parseTextSubstitutions choice
      <*> (Narrative <$> parseTextSubstitutions title <*> parseBodyContent)
  skipWhitespaceAndComments
  (danger_choice,danger_question,danger_narrative) ←
    withElement "danger" ["choice","title","question"] $ \[choice,title,question] → (,,)
      <$> parseTextSubstitutions choice
      <*> parseTextSubstitutions question
      <*> (Narrative <$> parseTextSubstitutions title <*> parseBodyContent)
  skipWhitespaceAndComments
  (averted_choice,averted_narrative) ←
    withElement "averted" ["choice","title"] $ \[choice,title] → (,)
      <$> parseTextSubstitutions choice
      <*> (Narrative <$> parseTextSubstitutions title <*> parseBodyContent)
  skipWhitespaceAndComments
  (failure_choice,failure_narrative) ←
    withElement "failure" ["choice","title"] $ \[choice,title] → (,)
      <$> parseTextSubstitutions choice
      <*> (Narrative <$> parseTextSubstitutions title <*> parseBodyContent)
  skipWhitespaceAndComments
  shames ← manyNonEmptyIgnoringSurroundingWhitespaceAndComments $
    withElement "shame" [] $ \[] → parseContent
  pure $ Event {..}

parseChoice ∷ Parser (Substitutions,StoryNode)
parseChoice = withElement "choice" ["selection"] $ \[selection] →
  (,) <$> parseTextSubstitutions selection <*> parseStoryNode

parseBranch ∷ Parser StoryNode
parseBranch = withElement "branch" ["title","question"] $ \[title_text,question_text] → do
  narrative ← Narrative <$> parseTextSubstitutions title_text <*> parseBodyContent
  question ← parseTextSubstitutions question_text
  choices ← sepEndByNonEmpty parseChoice skipWhitespaceAndComments
  pure $ BranchNode {..}

parseRandom ∷ Parser StoryNode
parseRandom = withElement "random" [] $ \[] → do
  optional skipWhitespaceAndComments
  RandomNode <$> sepEndByNonEmpty parseStoryNode skipWhitespaceAndComments

parseSequence ∷ Parser StoryNode
parseSequence = withElement "sequence" [] $ \[] → do
  skipWhitespaceAndComments
  substitutes ← sepEndBy parseSubstitute skipWhitespaceAndComments
  old_placeholders ← getState
  let placeholders = map Story.placeholder substitutes
      placeholders_as_set = HashSet.fromList placeholders
  unless (HashSet.size placeholders_as_set == length placeholders) $
    fail $ "Duplicate placeholders in " ⊕ (placeholders |> map unpack |> show)
  let conflicting_placeholders = HashSet.intersection placeholders_as_set old_placeholders
  unless (HashSet.null conflicting_placeholders) $
    fail $ "Conflicting placeholders " ⊕ (conflicting_placeholders |> HashSet.toList |> map unpack |> show)
  putState $ old_placeholders ⊕ placeholders_as_set
  fames ← sepEndBy parseFame skipWhitespaceAndComments
  stories ← sepEndByNonEmpty parseStoryNode skipWhitespaceAndComments
  putState old_placeholders
  pure $ SequenceNode{..}

runParserOnFile ∷ MonadIO m ⇒ FilePath → m (Either String StoryNode)
runParserOnFile filepath = liftIO $
  (
    (if isAbsolute filepath
      then pure filepath
      else getDataFileName ("stories" </> filepath)
    )
    >>=
    LB.readFile
    >>=
    (SAX.parseLocations SAX.defaultParseOptions >>> Parsec.runParserT (xmlDeclaration >> parseStoryNode) HashSet.empty filepath)
  )
  <&>
  first show

parseFile ∷ FilePath → Parser StoryNode
parseFile =
  (runParserOnFile >>> liftIO)
  >=>
  either (show >>> ("error parsing file: " ⊕) >>> fail) pure

parseFileElement ∷ Parser StoryNode
parseFileElement = withElement "file" ["path"] $ \[path] →
  parseFile (unpack path)

parseStoryNode ∷ Parser StoryNode
parseStoryNode = between skipWhitespaceAndComments skipWhitespaceAndComments $
      ((NarrativeOccurance >>> OccuranceNode) <$> parseNarrative)
  <|> ((EventOccurance >>> OccuranceNode) <$> parseEvent)
  <|> parseBranch
  <|> parseRandom
  <|> parseSequence
  <|> parseFileElement
