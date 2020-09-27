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
  , has_article_
  , is_uppercase_
  , kind_
  , key_
  , extractAllPlaceholders
  , extractPlaceholders
  ) where

import Control.Category ((>>>))
import Control.Lens ((^.),makeLenses)
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
  { _has_article_ ∷ Bool
  , _is_uppercase_ ∷ Bool
  , _kind_ ∷ Kind
  , _key_ ∷ Text
  }
  deriving (Eq,Ord,Read,Show)
makeLenses ''SubstitutionData

data Chunk α = Literal α | Substitution SubstitutionData
  deriving (Eq,Functor,Ord,Read,Show)
type instance Element Content = Chunk Text
newtype Content = Content { unwrapContent ∷ [Chunk Text] }
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
  mapMaybe (\case { Literal _ → Nothing; Substitution s → Just (s ^. key_) })
  >>>
  setFromList

extractAllPlaceholders ∷ Foldable f ⇒ f Content → HashSet Text
extractAllPlaceholders = foldMap extractPlaceholders
