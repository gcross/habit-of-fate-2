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
import Control.Monad ((>=>),forM_,unless,when)
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
  , sepBy
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
import HabitOfFate.Data.Gender
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

sepByNonEmpty ∷ Parser α → Parser sep → Parser (NonEmpty α)
sepByNonEmpty p psep =
  sepBy p psep
  >>=
  \case
    (x:xs) → pure (x:|xs)
    [] → fail "at least one element required"

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

parseSubstitute ∷ Parser Story
parseSubstitute = withElement "substitute" ["placeholder"] $ \[placeholder] → do
  old_placeholders ← getState
  when (HashSet.member placeholder old_placeholders) $
    fail $ "a substitution already exists for " ⊕ unpack placeholder
  putState $ HashSet.insert placeholder old_placeholders
  Substitute placeholder <$> manyNonEmptyIgnoringSurroundingWhitespaceAndComments parseGendered

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

parseNarrative ∷ Parser Story
parseNarrative = withElement "narrative" ["title"] $ \[title] →
  Narrative <$> parseTextSubstitutions title <*> parseBodyContent

parseEvent ∷ Parser Story
parseEvent = withElement "event" ["title","question"] $ \[event_title,event_question] → do
  common_title ← parseTextSubstitutions event_title
  common_question ← parseTextSubstitutions event_question
  common_content ← parseBodyContent
  skipWhitespaceAndComments
  (success_choice,success_title,success_content) ←
    withElement "success" ["choice","title"] $ \[choice,title] → (,,)
      <$> parseTextSubstitutions choice
      <*> parseTextSubstitutions title
      <*> parseBodyContent
  skipWhitespaceAndComments
  (danger_choice,danger_title,danger_question,danger_content) ←
    withElement "danger" ["choice","title","question"] $ \[choice,title,question] → (,,,)
      <$> parseTextSubstitutions choice
      <*> parseTextSubstitutions title
      <*> parseTextSubstitutions question
      <*> parseBodyContent
  skipWhitespaceAndComments
  (averted_choice,averted_title,averted_content) ←
    withElement "averted" ["choice","title"] $ \[choice,title] → (,,)
      <$> parseTextSubstitutions choice
      <*> parseTextSubstitutions title
      <*> parseBodyContent
  skipWhitespaceAndComments
  (failure_choice,failure_title,failure_content) ←
    withElement "failure" ["choice","title"] $ \[choice,title] → (,,)
      <$> parseTextSubstitutions choice
      <*> parseTextSubstitutions title
      <*> parseBodyContent
  skipWhitespaceAndComments
  pure $ Event {..}

parseChoice ∷ Parser (Substitutions,Story)
parseChoice = withElement "choice" ["selection"] $ \[selection] →
  (,) <$> parseTextSubstitutions selection <*> parseStory

parseBranch ∷ Parser Story
parseBranch = withElement "branch" ["title","question"] $ \[title_text,question_text] → do
  title ← parseTextSubstitutions title_text
  content ← parseBodyContent
  question ← parseTextSubstitutions question_text
  choices ← sepEndByNonEmpty parseChoice skipWhitespaceAndComments
  pure $ Branch {..}

parseOrderAttribute ∷ Text → Parser Order
parseOrderAttribute "sequential" = pure Sequential
parseOrderAttribute "random" = pure Random
parseOrderAttribute _ = fail "bad ordering specified"

parseCollection ∷ Parser Story
parseCollection = withElement "collection" ["order"] $ \[order_text] → do
  old_placeholders ← getState
  optional skipWhitespaceAndComments
  collection ←
    Collection
      <$> parseOrderAttribute order_text
      <*> sepByNonEmpty parseStory skipWhitespaceAndComments
  putState old_placeholders
  pure collection

runParserOnFile ∷ MonadIO m ⇒ FilePath → m (Either String Story)
runParserOnFile filepath = liftIO $
  (
    (if isAbsolute filepath
      then pure filepath
      else getDataFileName ("stories" </> filepath)
    )
    >>=
    LB.readFile
    >>=
    (SAX.parseLocations SAX.defaultParseOptions >>> Parsec.runParserT (xmlDeclaration >> parseStory) HashSet.empty filepath)
  )
  <&>
  first show

parseFile ∷ FilePath → Parser Story
parseFile =
  (runParserOnFile >>> liftIO)
  >=>
  either (show >>> ("error parsing file: " ⊕) >>> fail) pure

parseFileElement ∷ Parser Story
parseFileElement = withElement "file" ["path"] $ \[path] →
  parseFile (unpack path)

parseStory ∷ Parser Story
parseStory = between skipWhitespaceAndComments skipWhitespaceAndComments $
      parseSubstitute
  <|> parseNarrative
  <|> parseEvent
  <|> parseBranch
  <|> parseCollection
  <|> parseFileElement

