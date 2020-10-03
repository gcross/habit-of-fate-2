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

import Data.Text (Text)

import HabitOfFate.Data.Content (Content)
import HabitOfFate.Data.Gender (Gendered)

data Order = Sequential | Random
  deriving (Bounded,Enum,Eq,Ord,Read,Show)

data Story =
    Substitute
    { placeholder ∷ Text
    , candidates ∷ [Gendered]
    }
  | Narrative
    { title ∷ Content
    , content ∷ Content
    }
  | Event
    { common_title ∷ Content
    , common_content ∷ Content
    , common_question ∷ Content
    , success_choice ∷ Content
    , success_title ∷ Content
    , success_content ∷ Content
    , danger_choice ∷ Content
    , danger_title ∷ Content
    , danger_content ∷ Content
    , danger_question ∷ Content
    , averted_choice ∷ Content
    , averted_title ∷ Content
    , averted_content ∷ Content
    , failure_choice ∷ Content
    , failure_title ∷ Content
    , failure_content ∷ Content
    }
  | Branch
    { title ∷ Content
    , content ∷ Content
    , question ∷ Content
    , choices ∷ [(Content,Story)]
    }
  | Collection
    { order ∷ Order
    , stories ∷ [Story]
    }
  deriving (Eq,Ord,Read,Show)
