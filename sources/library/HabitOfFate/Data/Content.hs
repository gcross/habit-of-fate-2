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

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Content
  ( Chunk(..)
  , Content(..)
  , Kind(..)
  , Referrent(..)
  , SubstitutionData(..)
  , extractAllPlaceholders
  , extractPlaceholders
  ) where

import Control.Category ((>>>))
import Control.Lens ((&),makeLenses)
import Data.Containers (setFromList)
import Data.Default (Default)
import Data.HashSet (HashSet)
import Data.Maybe (mapMaybe)
import Data.MonoTraversable (Element,MonoFoldable,MonoPointed)
import Data.Sequences (singleton)
import Data.String (IsString(..))
import Data.Text (Text)

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
makeLenses ''SubstitutionData

data Chunk = Literal Text | Substitution SubstitutionData
  deriving (Eq,Ord,Read,Show)
type instance Element Content = Chunk
newtype Content = Content { unwrapContent ∷ [Chunk] }
  deriving
    ( Default
    , Eq
    , MonoFoldable
    , MonoPointed
    , Monoid
    , Ord
    , Read
    , Semigroup
    , Show
    )

instance IsString Content where
  fromString = fromString >>> Literal >>> singleton

extractPlaceholders ∷ Content → HashSet Text
extractPlaceholders =
  unwrapContent
  >>>
  mapMaybe (\case { Substitution s → Just (s & placeholder); _ → Nothing })
  >>>
  setFromList

extractAllPlaceholders ∷ Foldable f ⇒ f Content → HashSet Text
extractAllPlaceholders = foldMap extractPlaceholders
