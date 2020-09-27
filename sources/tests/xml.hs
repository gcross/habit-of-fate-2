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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.CallStack (HasCallStack)
import System.FilePath (FilePath)
import System.IO (hClose,hFlush,hPutStr,hPutStrLn,hSetEncoding,utf8)
import System.IO.Temp (withSystemTempFile)

import HabitOfFate.Data.Content
import HabitOfFate.Data.Story
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions
import HabitOfFate.XML

runParserOnString ∷ String → IO (Either String Story)
runParserOnString contents = withSystemTempFile "story" $ \filepath handle → do
  hSetEncoding handle utf8
  hPutStrLn handle "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  hPutStrLn handle contents
  hFlush handle
  hClose handle
  runParserOnFile filepath

main ∷ HasCallStack ⇒ IO ()
main = doMain
  [ testGroup "singleton tags"
    [ testCase "narrative" $
        runParserOnString "<narrative title=\"stuff\">happens</narrative>"
        >>=
        (@?= Right (Narrative "stuff" "happens"))
    ]
  ]
