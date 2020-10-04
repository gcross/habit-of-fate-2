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

module HabitOfFate.Data.Content (Content(..)) where

import Control.Category ((>>>))
import Data.String (IsString(..))

import HabitOfFate.Data.Substitutions

newtype Content = Unformatted Substitutions
  deriving (Eq,Ord,Read,Show)

instance IsString Content where
  fromString = fromString >>> Unformatted
