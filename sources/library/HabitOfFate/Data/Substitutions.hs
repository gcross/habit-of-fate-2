{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2019 Gregory Crosswhite

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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Substitutions
  ( Kind(..)
  , Referrent(..)
  , Substitutions(..)
  , SubstitutionChunk(..)
  , SubstitutionData(..)
  , extractAllPlaceholders
  , extractPlaceholders
  ) where

import Control.Category ((>>>))
import Control.Lens ((&))
import Data.Containers (setFromList)
import Data.HashSet (HashSet)
import Data.Maybe (mapMaybe)
import Data.String (IsString(..))
import Data.Text (Text,pack)

data Referrent =
    Subject
  | Object
  | Possessive
  | ProperPossessive
  | Reflexive
  | Category
  | CategoryPlural
  | Offspring
  | OffspringPlural
  | Marital
  | MaritalPlural
  deriving (Bounded,Enum,Eq,Ord,Read,Show)

data Kind =
    Name
  | Referrent Referrent
  deriving (Eq,Ord,Read,Show)

data SubstitutionData = SubstitutionData
  { has_article ∷ Bool
  , is_uppercase ∷ Bool
  , kind ∷ Kind
  , placeholder ∷ Text
  }
  deriving (Eq,Ord,Read,Show)

data SubstitutionChunk = Literal Text | Substitution SubstitutionData
  deriving (Eq,Ord,Read,Show)
newtype Substitutions = Substitutions { unwrapSubstitutions ∷ [SubstitutionChunk] }
  deriving (Eq,Ord,Read,Show)

instance IsString Substitutions where
  fromString = pack >>> Literal >>> (:[]) >>> Substitutions

extractPlaceholders ∷ Substitutions → HashSet Text
extractPlaceholders =
  unwrapSubstitutions
  >>>
  mapMaybe (\case { Substitution s → Just (s & placeholder); _ → Nothing })
  >>>
  setFromList

extractAllPlaceholders ∷ Foldable f ⇒ f Substitutions → HashSet Text
extractAllPlaceholders = foldMap extractPlaceholders
