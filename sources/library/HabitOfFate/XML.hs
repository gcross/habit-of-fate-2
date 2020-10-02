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
import Control.Lens (Lens',Prism',(&),(^.),(^?),(<<?~),(<&>),makeLenses,prism')
import Control.Lens.Extras (is)
import Control.Monad ((>=>),unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as LB
import Data.List ((\\))
import Data.Maybe (fromJust,isNothing)
import qualified Data.Text as Text
import Data.Text (Text,unpack)
import Flow ((|>))
import qualified Text.Parsec as Parsec
import Text.Parsec
  ( ParsecT
  , (<|>)
  , between
  , optional
  , sepBy1
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
import HabitOfFate.Operators ((⊕),(∈))
import HabitOfFate.Substitution

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

type Parser = ParsecT [(XMLEvent,XMLParseLocation)] () IO

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

whitespace ∷ Parser ()
whitespace = parseToken $
  (^? _CharacterData)
  >=>
  (Text.all (∈ (" \t\r\n" ∷ Text)) >>> bool Nothing (Just ()))

comment ∷ Parser ()
comment = parseToken (is _Comment >>> bool Nothing (Just ()))

skipComments ∷ Parser ()
skipComments = skipMany comment

skipWhitespaceAndComments ∷ Parser ()
skipWhitespaceAndComments = skipMany (whitespace <|> comment)

many1IgnoringSurroundingWhitespaceAndComments ∷ Parser α → Parser [α]
many1IgnoringSurroundingWhitespaceAndComments p = do
  optional skipWhitespaceAndComments
  sepEndBy1 p skipWhitespaceAndComments

characterData ∷ Parser Text
characterData = parseToken $ (^? _CharacterData)

characterDataSkippingComments ∷ Parser Text
characterDataSkippingComments = mconcat <$> sepEndBy characterData skipComments

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

parseCandidate ∷ Parser Candidate
parseCandidate = withElement "candidate" ["name","gender"] $ \[name,gender_string] →
  Candidate name <$> parseGender gender_string

parseSubstitute ∷ Parser Story
parseSubstitute = withElement "substitute" ["placeholder"] $ \[placeholder] →
  Substitute placeholder <$> many1IgnoringSurroundingWhitespaceAndComments parseCandidate

parseContent ∷ Parser Content
parseContent = characterDataSkippingComments >>= parseContentFromText

parseContentFromText ∷ Text → Parser Content
parseContentFromText = parseSubstitutions >>> either (show >>> fail) pure

parseNarrative ∷ Parser Story
parseNarrative = withElement "narrative" ["title"] $ \[title] →
  Narrative <$> parseContentFromText title <*> parseContent

data NonDangerElement = NonDangerElement
  { _nondanger_choice_ ∷ Content
  , _nondanger_title_ ∷ Content
  , _nondanger_content_ ∷ Content
  }
makeLenses ''NonDangerElement

parseNonDangerElement ∷ Text → Parser NonDangerElement
parseNonDangerElement tag = withElement tag ["choice","title"] $ \[choice,title] → do
  _nondanger_choice_ ← parseContentFromText choice
  _nondanger_title_ ← parseContentFromText title
  _nondanger_content_ ← parseContent
  pure $ NonDangerElement{..}

data DangerElement = DangerElement
  { _danger_choice_ ∷ Content
  , _danger_title_ ∷ Content
  , _danger_content_ ∷ Content
  , _danger_question_ ∷ Content
  }
makeLenses ''DangerElement

parseDangerElement ∷ Parser DangerElement
parseDangerElement = withElement "danger" ["choice","title","question"] $ \[choice,title,question] → do
  _danger_choice_ ← parseContentFromText choice
  _danger_title_ ← parseContentFromText title
  _danger_content_ ← parseContent
  _danger_question_ ← parseContentFromText question
  pure $ DangerElement{..}

data EventParseState = EventParseState
  { _success_element_ ∷ Maybe NonDangerElement
  , _danger_element_ ∷ Maybe DangerElement
  , _averted_element_ ∷ Maybe NonDangerElement
  , _failure_element_ ∷ Maybe NonDangerElement
  }
makeLenses ''EventParseState

updateElement ∷ Text → Lens' EventParseState (Maybe α) → Parser α → EventParseState → Parser EventParseState
updateElement element_name element_ parseNewValue old_event_parse_state = do
  new_value ← parseNewValue
  let (maybe_old_value,new_state) = old_event_parse_state & element_ <<?~ new_value
  unless (isNothing maybe_old_value) $ fail $ "more than one " ⊕ unpack element_name ⊕ " element found"
  pure new_state

parseEvent ∷ Parser Story
parseEvent = do
  [title,question] ← startElementWithAttributes "event" ["title","question"]
  common_title ← parseContentFromText title
  common_content ← parseContent
  common_question ← parseContentFromText question
  let go ∷ EventParseState → Parser EventParseState
      go old_event_parse_state =
              parseAndUpdateNonDangerElement "success" success_element_
          <|> parseAndUpdateNonDangerElement "averted" success_element_
          <|> parseAndUpdateNonDangerElement "failure" success_element_
          <|> (updateElement "danger" danger_element_ parseDangerElement old_event_parse_state >>= go)
          <|> (skipWhitespaceAndComments >> go old_event_parse_state)
          <|> (endElement "event" >> pure old_event_parse_state)
          where
           parseAndUpdateNonDangerElement ∷
             Text →
             Lens' EventParseState (Maybe NonDangerElement) →
             Parser EventParseState
           parseAndUpdateNonDangerElement name element_ =
             updateElement name element_ (parseNonDangerElement name) old_event_parse_state >>= go
  final_event_parse_state ← go $ EventParseState Nothing Nothing Nothing Nothing
  let checkForNonDangerElement ∷
        String →
        Lens' EventParseState (Maybe NonDangerElement) →
        Parser NonDangerElement
      checkForNonDangerElement element_name element_ =
        final_event_parse_state |> (^. element_) |> maybe (fail $ "no " ⊕ element_name ⊕ " element") pure
  success_element ← checkForNonDangerElement "success" success_element_
  averted_element ← checkForNonDangerElement "averted" averted_element_
  failure_element ← checkForNonDangerElement "failure" failure_element_
  danger_element  ← final_event_parse_state |> (^. danger_element_) |> maybe (fail $ "no danger element") pure
  let success_choice  = success_element ^. nondanger_choice_
      success_title   = success_element ^. nondanger_title_
      success_content = success_element ^. nondanger_content_

      averted_choice  = averted_element ^. nondanger_choice_
      averted_title   = averted_element ^. nondanger_title_
      averted_content = averted_element ^. nondanger_content_

      failure_choice  = failure_element ^. nondanger_choice_
      failure_title   = failure_element ^. nondanger_title_
      failure_content = failure_element ^. nondanger_content_

      danger_choice   = danger_element  ^. danger_choice_
      danger_title    = danger_element  ^. danger_title_
      danger_content  = danger_element  ^. danger_content_
      danger_question = danger_element  ^. danger_question_
  pure $ Event {..}

parseChoice ∷ Parser (Content,Story)
parseChoice = withElement "choice" ["selection"] $ \[selection] →
  (,) <$> parseContentFromText selection <*> parseStory

parseBranch ∷ Parser Story
parseBranch = withElement "branch" ["title","question"] $ \[title_text,question_text] → do
  title ← parseContentFromText title_text
  content ← parseContent
  question ← parseContentFromText question_text
  choices ← sepEndBy1 parseChoice skipWhitespaceAndComments
  pure $ Branch {..}

parseOrderAttribute ∷ Text → Parser Order
parseOrderAttribute "sequential" = pure Sequential
parseOrderAttribute "random" = pure Random
parseOrderAttribute _ = fail "bad ordering specified"

parseCollection ∷ Parser Story
parseCollection = withElement "collection" ["order"] $ \[order_text] →
  optional skipWhitespaceAndComments
  >>
  Collection
    <$> parseOrderAttribute order_text
    <*> sepBy1 parseStory skipWhitespaceAndComments

runParserOnFile ∷ MonadIO m ⇒ FilePath → m (Either String Story)
runParserOnFile filepath = liftIO $
  (
    LB.readFile filepath
    >>=
    (SAX.parseLocations SAX.defaultParseOptions >>> Parsec.runParserT (xmlDeclaration >> parseStory) () filepath)
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

