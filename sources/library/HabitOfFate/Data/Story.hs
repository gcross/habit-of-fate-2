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

module HabitOfFate.Data.Story
  ( Branch(..)
  , Collection(..)
  , Order(..)
  , Story(..)
  ) where

import HabitOfFate.Data.Event (Event)
import HabitOfFate.Data.Narrative (Narrative)
import HabitOfFate.Data.Substitute (Substitute)

data Branch content = Branch
  { _branch_narrative ∷ Narrative content
  , _branch_question_ ∷ content
  , _branch_choices ∷ [(content,Story content)]
  } deriving (Eq,Ord,Read,Show)

data Order = Sequential | Random
  deriving (Bounded,Enum,Eq,Ord,Read,Show)

data Collection content = Collection
  { _collection_order_ ∷ Order
  , _collection_stories_ ∷ [Story content]
  } deriving (Eq,Ord,Read,Show)

data Story content =
    SubstituteEntry Substitute
  | NarrativeEntry (Narrative content)
  | EventEntry (Event content)
  | BranchEntry (Branch content)
  | CollectionEntry (Collection content)
  deriving (Eq,Ord,Read,Show)
