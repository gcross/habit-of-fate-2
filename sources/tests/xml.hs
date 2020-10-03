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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.CallStack (HasCallStack)
import Data.String.Interpolate (i)
import System.FilePath (FilePath)
import System.IO (hClose,hFlush,hPutStr,hPutStrLn,hSetEncoding,utf8)
import System.IO.Temp (withSystemTempFile)

import HabitOfFate.Data.Content
import HabitOfFate.Data.Gender
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
    [ testCase "substitute" $
        runParserOnString "<substitute placeholder=\"hole\"><candidate name=\"Ander\" gender=\"male\"/></substitute>"
        >>=
        (@?= Right (Substitute "hole" [Candidate "Ander" Male]))
    , testCase "narrative" $
        runParserOnString "<narrative title=\"stuff\">happens</narrative>"
        >>=
        (@?= Right (Narrative "stuff" "happens"))
    , testCase "event" $
        runParserOnString "<event title=\"eve\" question=\"quest\">common<success choice=\"right\" title=\"good\">yay</success><danger choice=\"gamble\" title=\"possibly\" question=\"roll\">feeling lucky</danger><averted choice=\"soso\" title=\"maybe\">okay</averted><failure choice=\"wrong\" title=\"bad\">no</failure></event>"
        >>=
        (@?= let common_title = "eve"
                 common_content = "common"
                 common_question = "quest"
                 success_choice = "right"
                 success_title = "good"
                 success_content = "yay"
                 danger_choice = "gamble"
                 danger_title = "possibly"
                 danger_content = "feeling lucky"
                 danger_question = "roll"
                 averted_choice = "soso"
                 averted_title = "maybe"
                 averted_content = "okay"
                 failure_choice = "wrong"
                 failure_title = "bad"
                 failure_content = "no"
             in Right (Event{..})
        )
    , testCase "branch" $
        runParserOnString "<branch title=\"stuff\" question=\"why?\">story time<choice selection=\"because\"><narrative title=\"answer\">so it would seem</narrative></choice></branch>"
        >>=
        (@?= Right (Branch "stuff" "story time" "why?" [("because",Narrative "answer" "so it would seem")]))
    , testCase "collection" $
        runParserOnString "<collection order=\"random\"><narrative title=\"title\">content</narrative></collection>"
        >>=
        (@?= Right (Collection Random [Narrative "title" "content"]))
    ]
  , testGroup "whitespace and comments ignored when appropriate"
    [ testCase "substitute/candidate" $
        runParserOnString [i|
<substitute placeholder="hole"><!-- comment -->
  <candidate name="Ander" gender="male"/><!-- comment -->
</substitute>
|]
        >>=
        (@?= Right (Substitute "hole" [Candidate "Ander" Male]))
    , testCase "narrative" $
        runParserOnString [i|
<narrative title="stuff"><!-- comment -->happens<!-- comment --></narrative>
|]
        >>=
        (@?= Right (Narrative "stuff" "happens"))
    , testCase "event" $
        runParserOnString [i|
<event title="eve" question="quest"><!-- comment -->
  common<!-- comment -->
  <success choice="right" title="good"><!-- comment -->
    yay<!-- comment -->
  </success><!-- comment -->
  <danger choice="gamble" title="possibly" question="roll"><!-- comment -->
    feeling lucky<!-- comment -->
  </danger><!-- comment -->
  <averted choice="soso" title="maybe"><!-- comment -->
    okay<!-- comment -->
  </averted><!-- comment -->
  <failure choice="wrong" title="bad"><!-- comment -->
    no<!-- comment -->
  </failure><!-- comment -->
</event>
|]
        >>=
        (@?= let common_title = "eve"
                 common_content = "\n  common\n  "
                 common_question = "quest"
                 success_choice = "right"
                 success_title = "good"
                 success_content = "\n    yay\n  "
                 danger_choice = "gamble"
                 danger_title = "possibly"
                 danger_content = "\n    feeling lucky\n  "
                 danger_question = "roll"
                 averted_choice = "soso"
                 averted_title = "maybe"
                 averted_content = "\n    okay\n  "
                 failure_choice = "wrong"
                 failure_title = "bad"
                 failure_content = "\n    no\n  "
             in Right (Event{..})
        )
    , testCase "branch/choice" $
        runParserOnString [i|
<branch title="stuff" question="why?"><!-- comment -->
  story time<!-- comment -->
  <choice selection="because"><!-- comment -->
    <narrative title="answer">so it would seem<!-- comment --></narrative><!-- comment -->
  </choice><!-- comment -->
</branch>
|]
        >>=
        (@?= Right (Branch "stuff" "\n  story time\n  " "why?" [("because",Narrative "answer" "so it would seem")]))
    , testCase "collection" $
        runParserOnString [i|
<collection order="random"><!-- comment -->
  <narrative title="title1">content1<!-- comment --></narrative><!-- comment -->
  <narrative title="title2">content2<!-- comment --></narrative><!-- comment -->
</collection>
|]
        >>=
        (@?= Right (Collection Random [Narrative "title1" "content1", Narrative "title2" "content2"]))
    ]
  ]
