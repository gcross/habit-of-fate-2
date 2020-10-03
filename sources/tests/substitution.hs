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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.CallStack (HasCallStack)
import Data.Text (Text)

import HabitOfFate.Data.Content
import HabitOfFate.Data.Gender
import HabitOfFate.Substitution
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions

main ∷ HasCallStack ⇒ IO ()
main = doMain
  [ testGroup "parseSubstitutions"
    [ testGroup "singleton"
      [ testGroup "literal"
        [ testCase "one char" $
            parseSubstitutions "l" >>= (@?= Content [Literal "l"])
        , testCase "whole string" $
            parseSubstitutions "xyz" >>= (@?= Content [Literal "xyz"])
        ]
      , testGroup "substitution"
        [ testGroup "has_article" $
          let is_uppercase = False
              kind = Name
              placeholder = "name"
              test ∷ String → Text → Bool → TestTree
              test test_name text_to_parse has_article =
                testCase test_name $
                  parseSubstitutions text_to_parse
                  >>=
                  (@?= Content [Substitution SubstitutionData{..}])
          in
            [ test "name" "|name" False
            , test "name" "a |name" True
            , test "name" "an |name" True
            ]
        , testGroup "is_uppercase" $
          let has_article = False
              test ∷ String → Text → Text → Bool → Kind → TestTree
              test test_name text_to_parse placeholder is_uppercase kind =
                testCase test_name $
                  parseSubstitutions text_to_parse
                  >>=
                  (@?= Content [Substitution SubstitutionData{..}])
          in
            [ test "|name" "|name" "name" False Name
            , test "|Name" "|Name" "Name" True Name
            , test "he/she|name" "he/she|name" "name" False $ Referrent Subject
            , test "he/she|Name" "he/she|Name" "Name" False $ Referrent Subject
            , test "He/she|name" "He/she|name" "name" True $ Referrent Subject
            , test "He/she|Name" "He/she|Name" "Name" True $ Referrent Subject
            , test "He/She|name" "He/She|name" "name" True $ Referrent Subject
            , test "He/She|Name" "He/She|Name" "Name" True $ Referrent Subject
            ]
        , testGroup "kind" $
          let has_article = False
              is_uppercase = False
              placeholder = "name"
              test ∷ String → Text → Kind → TestTree
              test test_name text_to_parse kind =
                testCase test_name $
                  parseSubstitutions text_to_parse
                  >>=
                  (@?= Content [Substitution SubstitutionData{..}])
          in
            [ test "name" "|name" Name
            , test "subject" "he/she|name" $ Referrent Subject
            , test "possessive" "his/her|name" $ Referrent Possessive
            , test "proper possessive" "his/hers|name" $ Referrent ProperPossessive
            , test "reflexive" "himself/herself|name" $ Referrent Reflexive
            , test "category" "man/woman|name" $ Referrent Category
            , test "category plural" "men/women|name" $ Referrent CategoryPlural
            , test "offspring" "son/daughter|name" $ Referrent Offspring
            , test "offspring plural" "sons/daughters|name" $ Referrent OffspringPlural
            , test "marital" "husband/wife|name" $ Referrent Marital
            , test "marital plural" "husbands/wives|name" $ Referrent MaritalPlural
            ]
        , testCase "placeholder" $
          let has_article = False
              is_uppercase = False
              kind = Name
              placeholder = "name"
          in
            parseSubstitutions "|name"
            >>=
            (@?= Content [Substitution SubstitutionData{..}])
        ]
      ]
    ]
  , testGroup "substitute"
    [ testCase "subject" $
      let has_article = False
          is_uppercase = False
          kind = Referrent Subject
          placeholder = "name"
      in substitute (Gendered "value" Female) SubstitutionData{..} @?= "she"
    , testCase "subject" $
      let has_article = False
          is_uppercase = True
          kind = Referrent Subject
          placeholder = "name"
      in substitute (Gendered "value" Male) SubstitutionData{..} @?= "He"
    , testCase "possessive" $
      let has_article = False
          is_uppercase = False
          kind = Referrent Possessive
          placeholder = "name"
      in substitute (Gendered "value" Male) SubstitutionData{..} @?= "his"
    , testCase "proper possessive" $
      let has_article = False
          is_uppercase = False
          kind = Referrent ProperPossessive
          placeholder = "name"
      in substitute (Gendered "value" Female) SubstitutionData{..} @?= "hers"
    , testCase "with article \"a\"" $
      let has_article = True
          is_uppercase = False
          kind = Name
          placeholder = "name"
      in substitute (Gendered "cat" Female) SubstitutionData{..} @?= "a cat"
    , testCase "with article \"an\"" $
      let has_article = True
          is_uppercase = False
          kind = Name
          placeholder = "name"
      in substitute (Gendered "apple" Neuter) SubstitutionData{..} @?= "an apple"
    , testCase "with capitalized article \"a\"" $
      let has_article = True
          is_uppercase = True
          kind = Name
          placeholder = "name"
      in substitute (Gendered "cat" Female) SubstitutionData{..} @?= "A cat"
    , testCase "with capitalized article \"an\"" $
      let has_article = True
          is_uppercase = True
          kind = Name
          placeholder = "name"
      in substitute (Gendered "apple" Neuter) SubstitutionData{..} @?= "An apple"
    ]
  ]
