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

{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Category ((>>>))
import qualified Data.Text.Lazy.IO as LIO
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Text.Blaze.Html.Renderer.Text (renderHtml)

import HabitOfFate.HTML
import HabitOfFate.Story

main ∷ IO ()
main = do
  arguments ← getArgs
  directory ← case arguments of
    [directory] → pure directory
    _ → do
      putStrLn "Must pass in a single argument to the directory where files will be generated."
      exitFailure
  createDirectoryIfMissing True directory
  loadStory >>= (generatePages >>> mapM_ (\(filename,html) → LIO.writeFile (directory </> filename) (renderHtml html)))
