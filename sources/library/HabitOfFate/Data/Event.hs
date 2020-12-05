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

import HabitOfFate.Data.Content (BodyContent,Content)
import HabitOfFate.Data.Substitutions (Substitutions)

data Event = Event
    { common_title ∷ Substitutions
    , common_content ∷ BodyContent
    , common_question ∷ Substitutions
    , success_choice ∷ Substitutions
    , success_title ∷ Substitutions
    , success_content ∷ BodyContent
    , danger_choice ∷ Substitutions
    , danger_title ∷ Substitutions
    , danger_content ∷ BodyContent
    , danger_question ∷ Substitutions
    , averted_choice ∷ Substitutions
    , averted_title ∷ Substitutions
    , averted_content ∷ BodyContent
    , failure_choice ∷ Substitutions
    , failure_title ∷ Substitutions
    , failure_content ∷ BodyContent
    , shames ∷ NonEmpty Content
    } deriving (Eq,Ord,Read,Show)
