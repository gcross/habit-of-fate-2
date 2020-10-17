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

{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Content (BodyContent(..),Content(..),ContentChunk(..)) where

import Control.Category ((>>>))
import Data.String (IsString(..))

import HabitOfFate.Data.Substitutions

data ContentChunk = Unformatted Substitutions | Bold Content
  deriving (Eq,Ord,Read,Show)

instance IsString ContentChunk where
  fromString = fromString >>> Unformatted

newtype Content = Content { unwrapContent ∷ [ContentChunk] }
  deriving (Eq,Ord,Read,Show)

instance IsString Content where
  fromString = fromString >>> (:[]) >>> Content

newtype BodyContent = BodyContent { unwrapBodyContent ∷ [Content] }
  deriving (Eq,Ord,Read,Show)

instance IsString BodyContent where
  fromString = fromString >>> (:[]) >>> BodyContent
