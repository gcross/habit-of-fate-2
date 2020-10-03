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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Gender (Gender(..),Gendered(..)) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), object, withObject, withText)
import Data.String.Interpolate (i)
import Data.Text (Text)

import HabitOfFate.JSON

data Gender = Male | Female | Neuter deriving (Enum,Eq,Ord,Read,Show)

instance ToJSON Gender where
  toJSON Male = String "male"
  toJSON Female = String "female"
  toJSON Neuter = String "neuter"

instance FromJSON Gender where
  parseJSON = withText "expected text for the gender" $ \case
    "male" → pure Male
    "female" → pure Female
    "neuter" → pure Neuter
    wrong → fail [i|gender must be "male", "female", or "neuter", not "#{wrong}"|]

data Gendered = Gendered
  { name ∷ Text
  , gender ∷ Gender
  } deriving (Eq,Ord,Read,Show)

instance ToJSON Gendered where
  toJSON Gendered{..} = object
    [ "name" .== toJSON name
    , "gender" .== toJSON gender
    ]

instance FromJSON Gendered where
  parseJSON = withObject "gendered entity must have object shape" $ \o →
    Gendered
      <$> (o .: "name" >>= parseJSON)
      <*> (o .: "gender" >>= parseJSON)
