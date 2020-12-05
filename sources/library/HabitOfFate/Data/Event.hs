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

module HabitOfFate.Data.Event (Event(..)) where

import Data.List.NonEmpty (NonEmpty)

import HabitOfFate.Data.Content
import HabitOfFate.Data.Narrative
import HabitOfFate.Data.Substitutions

data Event = Event
    { common_narrative ∷ Narrative
    , common_question ∷ Substitutions
    , success_choice ∷ Substitutions
    , success_narrative ∷ Narrative
    , danger_choice ∷ Substitutions
    , danger_narrative ∷ Narrative
    , danger_question ∷ Substitutions
    , averted_choice ∷ Substitutions
    , averted_narrative ∷ Narrative
    , failure_choice ∷ Substitutions
    , failure_narrative ∷ Narrative
    , shames ∷ NonEmpty Content
    } deriving (Eq,Ord,Read,Show)
