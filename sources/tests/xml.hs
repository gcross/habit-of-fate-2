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

import Control.Lens (_Left)
import Control.Lens.Extras (is)
import Data.CallStack (HasCallStack)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String.Interpolate (i)
import System.FilePath (FilePath)
import System.IO (hClose,hFlush,hPutStr,hPutStrLn,hSetEncoding,utf8)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty.HUnit ((@?))

import HabitOfFate.Data.Content
import HabitOfFate.Data.Gender
import HabitOfFate.Data.Story
import HabitOfFate.Data.Substitutions
import HabitOfFate.Operators ((⊕))
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions
import HabitOfFate.XML

withContentsInTemporaryFile ∷ String → (FilePath → IO α) → IO α
withContentsInTemporaryFile contents f = withSystemTempFile "story" $ \filepath handle → do
  hSetEncoding handle utf8
  hPutStrLn handle "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  hPutStrLn handle contents
  hFlush handle
  hClose handle
  f filepath

runParserOnString ∷ String → IO (Either String Story)
runParserOnString = flip withContentsInTemporaryFile runParserOnFile

main ∷ HasCallStack ⇒ IO ()
main = doMain
  [ testGroup "singleton tags"
    [ testCase "substitute" $
        runParserOnString "<substitute placeholder=\"hole\"><candidate name=\"Ander\" gender=\"male\"/></substitute>"
        >>=
        (@?= Right (Substitute "hole" (Gendered "Ander" Male:|[])))
    , testCase "narrative" $
        runParserOnString "<narrative title=\"stuff\"><p>happens</p></narrative>"
        >>=
        (@?= Right (Narrative "stuff" "happens"))
    , testCase "event" $
        runParserOnString "<event title=\"eve\" question=\"quest\"><p>common</p><success choice=\"right\" title=\"good\"><p>yay</p></success><danger choice=\"gamble\" title=\"possibly\" question=\"roll\"><p>feeling lucky</p></danger><averted choice=\"soso\" title=\"maybe\"><p>okay</p></averted><failure choice=\"wrong\" title=\"bad\"><p>no</p></failure></event>"
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
        runParserOnString "<branch title=\"stuff\" question=\"why?\"><p>story time</p><choice selection=\"because\"><narrative title=\"answer\"><p>so it would seem</p></narrative></choice></branch>"
        >>=
        (@?= Right (Branch "stuff" "story time" "why?" (("because",Narrative "answer" "so it would seem"):|[])))
    , testCase "collection" $
        runParserOnString "<collection order=\"random\"><narrative title=\"title\"><p>content</p></narrative></collection>"
        >>=
        (@?= Right (Collection Random (Narrative "title" "content":|[])))
    , testCase "file" $ withContentsInTemporaryFile "<narrative title=\"stuff\"><p>happens</p></narrative>" $ \filepath →
        runParserOnString ("<file path=\"" ⊕ filepath ⊕ "\"/>")
        >>=
        (@?= Right (Narrative "stuff" "happens"))
    ]
  , testGroup "whitespace and comments ignored when appropriate"
    [ testCase "substitute/candidate" $
        runParserOnString [i|
<substitute placeholder="hole"><!-- comment -->
  <candidate name="Ander" gender="male"/><!-- comment -->
</substitute>
|]
        >>=
        (@?= Right (Substitute "hole" (Gendered "Ander" Male:|[])))
    , testCase "narrative" $
        runParserOnString [i|
<narrative title="stuff"><!-- comment --><p>happens</p><!-- comment --></narrative>
|]
        >>=
        (@?= Right (Narrative "stuff" "happens"))
    , testCase "event" $
        runParserOnString [i|
<event title="eve" question="quest"><!-- comment -->
  <p>common</p><!-- comment -->
  <success choice="right" title="good"><!-- comment -->
    <p>yay<!-- comment --></p>
    <!-- comment -->
  </success><!-- comment -->
  <danger choice="gamble" title="possibly" question="roll"><!-- comment -->
    <p>feeling lucky<!-- comment --></p>
    <!-- comment -->
  </danger><!-- comment -->
  <averted choice="soso" title="maybe"><!-- comment -->
    <p>okay<!-- comment --></p>
    <!-- comment -->
  </averted><!-- comment -->
  <failure choice="wrong" title="bad"><!-- comment -->
    <p>no<!-- comment --></p>
    <!-- comment -->
  </failure><!-- comment -->
</event>
|]
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
    , testCase "branch/choice" $
        runParserOnString [i|
<branch title="stuff" question="why?"><!-- comment -->
  <p>story time</p><!-- comment -->
  <choice selection="because"><!-- comment -->
    <narrative title="answer"><p>so it would seem</p><!-- comment --></narrative><!-- comment -->
  </choice><!-- comment -->
</branch>
|]
        >>=
        (@?= Right (Branch "stuff" "story time" "why?" (("because",Narrative "answer" "so it would seem"):|[])))
    , testCase "collection" $
        runParserOnString [i|
<collection order="random"><!-- comment -->
  <narrative title="title1"><p>content1</p><!-- comment --></narrative><!-- comment -->
  <narrative title="title2"><p>content2</p><!-- comment --></narrative><!-- comment -->
</collection>
|]
        >>=
        (@?= Right (Collection Random (Narrative "title1" "content1":|Narrative "title2" "content2":[])))
    ]
  , testGroup "content formatting"
    [ testCase "bold" $
        runParserOnString "<narrative title=\"stuff\"><p><b>happens</b></p></narrative>"
        >>=
        (@?= Right (Narrative "stuff" $ BodyContent [Content [Bold "happens"]]))
    , testCase "mixed" $
        runParserOnString "<narrative title=\"format\"><p>regular<b>bold</b><b>BOLD</b>unbold</p></narrative>"
        >>=
        (@?= Right (Narrative "format" $ BodyContent [Content ["regular", Bold "bold", Bold "BOLD", "unbold"]]))
    , testCase "multiple paragraphs" $
        runParserOnString "<narrative title=\"stuff\"><p>1</p><p>2</p></narrative>"
        >>=
        (@?= Right (Narrative "stuff" $ BodyContent ["1","2"]))
    , testCase "multiple paragraphs with one bold" $
        runParserOnString "<narrative title=\"stuff\"><p><b>1</b></p><p>2</p></narrative>"
        >>=
        (@?= Right (Narrative "stuff" $ BodyContent [Content [Bold "1"],"2"]))
    ]
  , testGroup "substitutions"
    [ testCase "known placeholder" $
        runParserOnString [i|
<collection order="sequential">
  <substitute placeholder="Name">
    <candidate name="Ander" gender="male"/>
  </substitute>
  <narrative title="title1"><p>|Name</p></narrative>
</collection>
|]
        >>=
        (@?= Right
          (Collection Sequential
            (Substitute "Name" (Gendered "Ander" Male:|[])
            :|Narrative
              "title1"
              (BodyContent [Content
                [Unformatted
                  (Substitutions
                    [Substitution $ SubstitutionData
                      { has_article = False
                      , is_uppercase = True
                      , kind = Name
                      , placeholder = "Name"
                      }
                    ]
                  )
                ]
              ])
            :[]
            )
          )
        )
    , testCase "unknown placeholder" $
        runParserOnString [i|
<collection order="sequential">
  <narrative title="title1">|Name</narrative>
</collection>
|]
        >>=
        (\result → is _Left result @? ("Result was not Left: " ⊕ show result))
    ]
  ]
