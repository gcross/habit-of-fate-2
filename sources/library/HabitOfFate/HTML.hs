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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.HTML (generatePages) where

import Control.Category ((>>>))
import Control.Lens ((<&>),(&),(.~),_1,_head)
import Control.Monad (forM_)
import Control.Monad.Trans.State (State,evalState,get,put)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text,unpack)
import Flow ((|>))
import qualified Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5 ((!),Html,toHtml,toValue)
import qualified Text.Blaze.XHtml5.Attributes as A

import HabitOfFate.Data.Content
import HabitOfFate.Data.Event
import HabitOfFate.Data.Gender
import HabitOfFate.Data.Narrative
import HabitOfFate.Data.Story
import qualified HabitOfFate.Data.Substitutions as Substitutions
import HabitOfFate.Data.Substitutions
import HabitOfFate.Substitution
import HabitOfFate.Operators ((⊕))

type SubstitutionMap = HashMap Text Gendered

generateText ∷ SubstitutionMap → Substitutions → Text
generateText substitution_map =
  unwrapSubstitutions
  >>>
  map (
    \case
      Literal text → text
      Substitution substitution →
        substitution_map
        |> HashMap.lookup (substitution & Substitutions.placeholder)
        |> fromMaybe (error $ "missing substitution: " ⊕ unpack (substitution & Substitutions.placeholder))
        |> substitute
        |> ($ substitution)
  )
  >>>
  mconcat

generateContent ∷ SubstitutionMap → Content → Html
generateContent substitution_map =
  unwrapContent
  >>>
  map (\case
    Unformatted text → toHtml (generateText substitution_map text) ∷ Html
    Bold text → generateContent substitution_map text
  )
  >>>
  mconcat

generateBodyContent ∷ SubstitutionMap → BodyContent → Html
generateBodyContent substitution_map =
  unwrapBodyContent >>> map (generateContent substitution_map >>> H.p) >>> mconcat

type Links = [(FilePath,Substitutions)]

generatePages ∷ StoryNode → [(FilePath,Html)]
generatePages = go "0.html" Nothing >>> flip evalState mempty >>> (_head . _1 .~ "index.html")
 where
  go ∷ FilePath → Maybe FilePath → StoryNode → State SubstitutionMap [(FilePath,Html)]
  go current_filename maybe_next_filename story = do
    substitution_map ← get
    let continue_links, back_to_list_of_quests_links ∷ Links
        continue_links = case maybe_next_filename of
          Nothing → []
          Just next_filename → [(next_filename,"Continue")]
        back_to_list_of_quests_links = [("index.html","[Back to the list of quests]")]

        outputTitle ∷ Substitutions → Html
        outputTitle title = H.head $ H.title $ toHtml (generateText substitution_map title)

        outputBodyContent ∷ BodyContent → Html
        outputBodyContent body_content = H.div $ generateBodyContent substitution_map body_content

        outputLinks ∷ Links → Html
        outputLinks links = H.div $ H.ul $ forM_ links $ \(page,substitutions) → H.li $ H.a ! A.href (toValue page) $ toHtml $ generateText substitution_map substitutions

        pageWithLinks ∷ Substitutions → BodyContent → Links → Html
        pageWithLinks title body_content links = H.docTypeHtml $ do
          outputTitle title
          H.body $ do
            outputBodyContent body_content
            H.br
            outputLinks $ links ⊕ back_to_list_of_quests_links

        pageWithDefaultLinks ∷ Substitutions → BodyContent → Html
        pageWithDefaultLinks title body_content = pageWithLinks title body_content continue_links

        pageWithQuestion ∷ Substitutions → BodyContent → Substitutions → Links → Html
        pageWithQuestion title body_content question links = H.docTypeHtml $ do
          outputTitle title
          H.body $ do
            outputBodyContent body_content
            H.br
            H.div $ toHtml $ generateText substitution_map question
            outputLinks $ links ⊕ if current_filename == "0.html" then [] else back_to_list_of_quests_links

        generateStories (stories@(_:|rest_stories)) =
          sequence
            [ go child_current_filename child_maybe_next_filename child_story
            | child_story ← NonEmpty.toList stories
            | child_current_filename ← current_filename:rest_child_filenames
            | child_maybe_next_filename ← map Just rest_child_filenames ⊕ [maybe_next_filename]
            ]
          <&>
          concat
         where
          rest_child_filenames =
            [ show i ⊕ "-" ⊕ current_filename
            | i ← [(0 ∷ Int)..]
            | _ ← rest_stories
            ]

    case story of
      NarrativeNode (Narrative{..}) → pure [(current_filename,pageWithDefaultLinks title content)]
      EventNode (Event{..}) →
        let success_filename = "S-" ⊕ current_filename
            danger_filename = "D-" ⊕ current_filename
            averted_filename = "A-" ⊕ current_filename
            failure_filename = "F-" ⊕ current_filename

            back_to_the_start_of_event_links ∷ Links
            back_to_the_start_of_event_links = [(current_filename,"[Back to the start of the event.]")]

            progress_possible_links = continue_links ⊕ back_to_the_start_of_event_links
        in pure $
          [(current_filename,pageWithQuestion common_title common_content common_question
              [(success_filename,success_choice)
              ,(danger_filename,danger_choice)
              ])
          ,(success_filename,pageWithLinks success_title success_content progress_possible_links)
          ,(danger_filename,pageWithQuestion danger_title danger_content danger_question $
              [(averted_filename,averted_choice)
              ,(failure_filename,failure_choice)
              ] ⊕ back_to_the_start_of_event_links)
          ,(averted_filename,pageWithLinks averted_title averted_content progress_possible_links)
          ,(failure_filename,pageWithLinks failure_title failure_content back_to_the_start_of_event_links)
          ]
      BranchNode{..} →
        let choices_with_filenames =
              [ (show i ⊕ "-" ⊕ current_filename,selection,choice_story)
              | i ← [(0 ∷ Int)..]
              | (selection,choice_story) ← NonEmpty.toList choices
              ]
        in
          mapM (\(choice_filename,_,choice_story) → go choice_filename maybe_next_filename choice_story) choices_with_filenames
          <&>
          (
            concat
            >>>
            ((current_filename
            ,pageWithQuestion title content question
                [ (choice_filename,selection)
                | (choice_filename,selection,_) ← choices_with_filenames
                ]
            ):
            )
          )
      RandomNode{..} →
        generateStories stories
      SequenceNode{..} → do
        put $ substitution_map ⊕ HashMap.fromList [(reference,gendered) | Substitute reference (gendered :| _) ← substitutes]
        pages ← generateStories stories
        put substitution_map
        pure pages
