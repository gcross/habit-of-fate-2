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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Substitution
  (
  -- Exceptions
    SubstitutionException(..)
  , ParseError

  -- Regular types + lenses
  , Substitutions

  -- Functions
  , parseSubstitutions
  , substitute
  , substituteEverywhere
  ) where

import Control.Category ((>>>),(<<<))
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Lens (Lens',Traversal',(&),(<&>),(.~),(^.),(^?!),_head,lens)
import Data.Bool (bool)
import Data.Char (isUpper,toLower,toUpper)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Control.Monad (msum,when)
import Data.MonoTraversable (onull)
import Data.Sequences (singleton)
import qualified Data.Text as Text
import Data.Text (Text,pack)
import Flow ((|>))
import Text.Parsec hiding (uncons)

import HabitOfFate.Data.Content
import HabitOfFate.Data.Gender
import HabitOfFate.Data.Markdown
import HabitOfFate.Operators ((∈),(⊕))

uppercase_ ∷ Lens' Char Bool
uppercase_ = lens isUpper (\c → bool (toLower c) (toUpper c))
first_uppercase_ ∷ Traversal' Text Bool
first_uppercase_ = _head . uppercase_

isVowel ∷ Char → Bool
isVowel = (∈ ("aeiouAEIOU" ∷ Text))

referrents ∷ [(String, Referrent)]
referrents =
  concatMap
    (\(male, female, referrent) →
      [ (male ⊕ "/" ⊕ female, referrent)
      , ((male & _head . uppercase_ .~ True) ⊕ "/" ⊕ female, referrent)
      , ((male & _head . uppercase_ .~ True) ⊕ "/" ⊕ (female & _head . uppercase_ .~ True), referrent)
      ]
    )
    [ ("he", "she", Subject)
    , ("him", "her", Object)
    , ("his", "her", Possessive)
    , ("his", "hers", ProperPossessive)
    , ("himself", "herself", Reflexive)
    , ("man", "woman", Category)
    , ("men", "women", CategoryPlural)
    , ("son", "daughter", Offspring)
    , ("sons", "daughters", OffspringPlural)
    , ("husband", "wife", Marital)
    , ("husbands", "wives", MaritalPlural)
    ]

data HasArticle = HasArticle | HasNoArticle
  deriving (Bounded,Enum,Eq,Ord,Read,Show)

data Case = Upper | Lower
  deriving (Bounded,Enum,Eq,Ord,Read,Show)

type Parser = Parsec String ()

getCase ∷ Char → Case
getCase c
  | isUpper c = Upper
  | otherwise = Lower

parseSubstitutionChunk ∷ HasArticle → Maybe Case → Parser (Chunk Char)
parseSubstitutionChunk article maybe_case = do
  kind ←
    try (string "|" >> pure Name)
    <|>
    msum
      [ try (string word >> char '|') >> (pure $ Referrent referrent)
      | (word, referrent) ← referrents
      ]
  case_ ←
    maybe
      ((lookAhead letter <&> getCase) <|> pure Upper)
      pure
      maybe_case
  key ← many alphaNum <&> pack
  pure $ Substitution $
    SubstitutionData
      (article == HasArticle)
      (case_ == Upper)
      kind
      key

parseChunk ∷ Parser (Chunk Char)
parseChunk = do
  maybe_case_ ← (lookAhead letter <&> (getCase >>> Just)) <|> pure Nothing
  (try $ do
    _ ← char 'a' <|> char 'A'
    _ ← optional $ char 'n'
    _ ← many1 <<< choice $ map char " \t\r\n"
    parseSubstitutionChunk HasArticle maybe_case_
   )
    <|> parseSubstitutionChunk HasNoArticle maybe_case_
    <|> (anyToken <&> Literal)

instance Exception ParseError

parseSubstitutions ∷ MonadThrow m ⇒ String → m Content
parseSubstitutions content =
  (
    runParser (many parseChunk ∷ Parser [Chunk Char]) () "(content)" content
    |> either throwM pure
  )
  <&>
  (mergeChunks >>> map ((pack >>> Text.reverse) <$>) >>> Content)
 where
  mergeChunks ∷ [Chunk Char] → [Chunk [Char]]
  mergeChunks [] = []
  mergeChunks (first:rest) = go (singleton <$> first) rest
   where
    go ∷ Chunk [Char] → [Chunk Char] → [Chunk [Char]]
    go previous [] = previous:[]
    go previous (next@(Substitution _):inner_rest) = previous:go (singleton <$> next) inner_rest
    go previous@(Substitution _) (next:inner_rest) = previous:go (singleton <$> next) inner_rest
    go (Literal x) (Literal y:inner_rest) = go (Literal (y:x)) inner_rest

data SubstitutionException =
    NoSuchKeyException Text
  | EmptyNameException
  deriving (Eq,Ord,Read,Show)
instance Exception SubstitutionException

type Substitutions = HashMap Text Gendered

lookupAndApplySubstitution ∷ MonadThrow m ⇒ Substitutions → SubstitutionData → m Text
lookupAndApplySubstitution table s = do
  gendered@(Gendered name _) ←
    maybe
      (throwM $ NoSuchKeyException $ s ^. key_)
      pure
      (HashMap.lookup (s ^. key_) table)
  when (onull name) $ throwM EmptyNameException
  let article ∷ Text
      article
        | s ^. has_article_ =
            if isVowel (name ^?! _head)
              then "an "
              else "a "
        | otherwise = ""
      word = applyKind (s ^. kind_) gendered
  pure $
    (article ⊕ word) & first_uppercase_ .~ (s ^. is_uppercase_)

data KeyError = KeyError Text deriving (Show)
instance Exception KeyError

substitute ∷ MonadThrow m ⇒ Substitutions → Content → m Markdown
substitute table (Content text) =
  mapM
    (\case
      Literal l → pure l
      Substitution s → lookupAndApplySubstitution table s
    )
    text
  <&>
  (mconcat >>> Markdown)

substituteEverywhere ∷ (MonadThrow m, Traversable t) ⇒ Substitutions → t Content → m (t Markdown)
substituteEverywhere = substitute >>> mapM

applyKind ∷ Kind → Gendered → Text
applyKind Name (Gendered name _) = name
applyKind (Referrent referrent) (Gendered _ gender) =
  applyReferrent referrent gender

applyReferrent ∷ Referrent → Gender → Text

applyReferrent Subject Male = "he"
applyReferrent Subject Female = "she"
applyReferrent Subject Neuter = "it"

applyReferrent Object Male = "him"
applyReferrent Object Female = "her"
applyReferrent Object Neuter = "it"

applyReferrent Possessive Male = "his"
applyReferrent Possessive Female = "her"
applyReferrent Possessive Neuter = "its"

applyReferrent ProperPossessive Male = "his"
applyReferrent ProperPossessive Female = "hers"
applyReferrent ProperPossessive Neuter = "its"

applyReferrent Reflexive Male = "himself"
applyReferrent Reflexive Female = "herself"
applyReferrent Reflexive Neuter = "itsself"

applyReferrent Category Male = "man"
applyReferrent Category Female = "woman"
applyReferrent Category Neuter = "thing"

applyReferrent CategoryPlural Male = "men"
applyReferrent CategoryPlural Female = "women"
applyReferrent CategoryPlural Neuter = "things"

applyReferrent Offspring Male = "son"
applyReferrent Offspring Female = "daughter"
applyReferrent Offspring Neuter = "offspring"

applyReferrent OffspringPlural Male = "sons"
applyReferrent OffspringPlural Female = "daughters"
applyReferrent OffspringPlural Neuter = "offspring"

applyReferrent Marital Male = "husband"
applyReferrent Marital Female = "wife"
applyReferrent Marital Neuter = "spouse"

applyReferrent MaritalPlural Male = "husbands"
applyReferrent MaritalPlural Female = "wives"
applyReferrent MaritalPlural Neuter = "spouses"
