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

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Substitution
  -- Exceptions
  ( ParseError

  -- Functions
  , parseSubstitutions
  , substitute
  ) where

import Control.Category ((>>>),(<<<))
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Lens (Lens',Traversal',(&),(<&>),(.~),(^?!),_head,lens)
import Data.Bool (bool)
import Data.Char (isUpper,toLower,toUpper)
import Control.Monad (msum)
import Data.Text (Text,pack)
import Flow ((|>))
import Text.Parsec hiding (uncons)

import HabitOfFate.Data.Content
import HabitOfFate.Data.Gender
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

data Intermediate = IntermediateLetter Char | IntermediateSubstitution SubstitutionData

type Parser = Parsec Text ()

getCase ∷ Char → Case
getCase c
  | isUpper c = Upper
  | otherwise = Lower

parseSubstitutionData ∷ HasArticle → Maybe Case → Parser SubstitutionData
parseSubstitutionData article maybe_case = do
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
  pure $ SubstitutionData (article == HasArticle) (case_ == Upper) kind key

parseIntermediate ∷ Parser Intermediate
parseIntermediate = do
  maybe_case_ ← (lookAhead letter <&> (getCase >>> Just)) <|> pure Nothing
  (try $ do
    _ ← char 'a' <|> char 'A'
    _ ← optional $ char 'n'
    _ ← many1 <<< choice $ map char " \t\r\n"
    parseSubstitutionData HasArticle maybe_case_ <&> IntermediateSubstitution
   )
    <|> (parseSubstitutionData HasNoArticle maybe_case_ <&> IntermediateSubstitution)
    <|> (anyToken <&> IntermediateLetter)

instance Exception ParseError

parseSubstitutions ∷ MonadThrow m ⇒ Text → m Content
parseSubstitutions content =
  (
    runParser (many parseIntermediate ∷ Parser [Intermediate]) () "(content)" content
    |> either throwM pure
  )
  <&>
  (consolidateIntermediates >>> Content)
 where
  consolidateIntermediates ∷ [Intermediate] → [Chunk]
  consolidateIntermediates [] = []
  consolidateIntermediates (remaining@(next:rest)) = case next of
    IntermediateLetter _ →
      let (cs,rest_intermediates) = consolidateLiterals remaining
      in Literal (pack cs):consolidateIntermediates rest_intermediates
    IntermediateSubstitution substitution_data → Substitution substitution_data:consolidateIntermediates rest
   where
    consolidateLiterals ∷ [Intermediate] → (String,[Intermediate])
    consolidateLiterals (IntermediateLetter c:rest_to_consolidate) = (c:rest_cs,rest_intermediates)
     where
      (rest_cs,rest_intermediates) = consolidateLiterals rest_to_consolidate
    consolidateLiterals rest_intermediates = ([],rest_intermediates)

substitute ∷ Gendered → SubstitutionData → Text
substitute (gendered@Gendered{..}) SubstitutionData{..} =
  let article ∷ Text
      article
        | has_article =
            if isVowel (name ^?! _head)
              then "an "
              else "a "
        | otherwise = ""
      word = applyKind kind gendered
  in (article ⊕ word) & first_uppercase_ .~ is_uppercase

data KeyError = KeyError Text deriving (Show)
instance Exception KeyError

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
