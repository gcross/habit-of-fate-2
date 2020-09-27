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

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Event
  ( Event(..)
  , common_narrative_
  , common_question_
  , success_choice_
  , success_narrative_
  , danger_choice_
  , danger_narrative_
  , danger_question_
  , averted_choice_
  , averted_narrative_
  , failure_choice_
  , failure_narrative_
  ) where

import Control.Lens (makeLenses)
import Data.Default (Default(..))

import HabitOfFate.Data.Narrative (Narrative)

data Event content = Event
  { _common_narrative_ ∷ Narrative content
  , _common_question_ ∷ content
  , _success_choice_ ∷ content
  , _success_narrative_ ∷ Narrative content
  , _danger_choice_ ∷ content
  , _danger_narrative_ ∷ Narrative content
  , _danger_question_ ∷ content
  , _averted_choice_ ∷ content
  , _averted_narrative_ ∷ Narrative content
  , _failure_choice_ ∷ content
  , _failure_narrative_ ∷ Narrative content
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)
instance Default content ⇒ Default (Event content) where
  def = Event def def def def def def def def def def def

makeLenses ''Event
