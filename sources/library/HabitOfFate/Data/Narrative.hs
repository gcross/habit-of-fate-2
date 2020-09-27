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

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Narrative (Narrative(..),title_,content_) where

import Control.Lens (makeLenses)
import Data.Default (Default(..))

data Narrative content = Narrative
  { _title_ ∷ content
  , _content_ ∷ content
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)
instance Default content ⇒ Default (Narrative content) where
  def = Narrative def def

makeLenses ''Narrative
