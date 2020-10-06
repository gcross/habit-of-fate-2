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

{-# OPTIONS_GHC -Wno-partial-fields #-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Story (Order(..),Story(..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import HabitOfFate.Data.Content (Content)
import HabitOfFate.Data.Gender (Gendered)
import HabitOfFate.Data.Substitutions (Substitutions)

data Order = Sequential | Random
  deriving (Bounded,Enum,Eq,Ord,Read,Show)

data Story =
    Substitute
    { placeholder ∷ Text
    , candidates ∷ [Gendered]
    }
  | Narrative
    { title ∷ Substitutions
    , content ∷ Content
    }
  | Event
    { common_title ∷ Substitutions
    , common_content ∷ Content
    , common_question ∷ Substitutions
    , success_choice ∷ Substitutions
    , success_title ∷ Substitutions
    , success_content ∷ Content
    , danger_choice ∷ Substitutions
    , danger_title ∷ Substitutions
    , danger_content ∷ Content
    , danger_question ∷ Substitutions
    , averted_choice ∷ Substitutions
    , averted_title ∷ Substitutions
    , averted_content ∷ Content
    , failure_choice ∷ Substitutions
    , failure_title ∷ Substitutions
    , failure_content ∷ Content
    }
  | Branch
    { title ∷ Substitutions
    , content ∷ Content
    , question ∷ Substitutions
    , choices ∷ [(Substitutions,Story)]
    }
  | Collection
    { order ∷ Order
    , stories ∷ NonEmpty Story
    }
  deriving (Eq,Ord,Read,Show)
