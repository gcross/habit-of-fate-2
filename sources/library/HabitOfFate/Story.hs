{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story (loadStory) where

import Control.Category ((>>>))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text,unpack)
import System.FilePath ((</>))

import HabitOfFate.Data.Gender (Gender(..),Gendered(..))
import HabitOfFate.Data.Story (Story(..))
import HabitOfFate.Data.Substitutions (SubstitutionChunk(..),Substitutions(..))
import HabitOfFate.Operators ((⊕))
import HabitOfFate.Substitution (substitute)
import HabitOfFate.XML (runParserOnFile)

import Paths_habit_of_fate

pathsWithoutFame ∷ Story → [Text]
pathsWithoutFame = (:[]) >>> go ""
 where
  pathFromSubstitutions =
    unwrapSubstitutions
    >>>
    map (\case
      Literal text → text
      Substitution s_data → substitute (Gendered "(thing)" Neuter) s_data
    )
    >>>
    mconcat

  go ∷ Text → [Story] → [Text]
  go path (Branch{..}:rest) =
    (traverse
      (\(selection,choice) → go (path ⊕ "/" ⊕ pathFromSubstitutions selection) (choice:[]))
      (NonEmpty.toList choices)
    )
    >>=
    (mconcat >>> flip go rest)
  go path (Sequence{..}:rest)
    | null fames =
        go path (NonEmpty.toList stories)
        >>=
        flip go rest
    | otherwise = mempty
  go path [] = pure path
  go path (_:rest) = go path rest

loadStory ∷ IO Story
loadStory = do
  story ←
    getDataFileName ("stories" </> "root.xml")
    >>=
    runParserOnFile
    >>=
    either fail pure
  case pathsWithoutFame story of
    [] → pure story
    paths → fail $ "paths without fames: " ⊕ show (map unpack paths)
