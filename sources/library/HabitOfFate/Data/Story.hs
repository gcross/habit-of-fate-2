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

module HabitOfFate.Data.Story (StoryNode(..),Substitute(..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import HabitOfFate.Data.Content (BodyContent,Content)
import HabitOfFate.Data.Event
import HabitOfFate.Data.Gender (Gendered)
import HabitOfFate.Data.Narrative (Narrative)
import HabitOfFate.Data.Substitutions (Substitutions)

data Substitute = Substitute
  { placeholder ∷ Text
  , candidates ∷ NonEmpty Gendered
  }
  deriving (Eq,Ord,Read,Show)

data StoryNode =
    NarrativeNode Narrative
  | EventNode Event
  | BranchNode
    { title ∷ Substitutions
    , content ∷ BodyContent
    , question ∷ Substitutions
    , choices ∷ NonEmpty (Substitutions,StoryNode)
    }
  | RandomNode
    { stories ∷ NonEmpty StoryNode
    }
  | SequenceNode
    { substitutes ∷ [Substitute]
    , fames ∷ [Content]
    , stories ∷ NonEmpty StoryNode
    }
  deriving (Eq,Ord,Read,Show)
