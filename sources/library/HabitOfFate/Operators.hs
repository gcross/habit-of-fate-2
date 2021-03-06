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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Operators ((∈),(⊕)) where

import Data.MonoTraversable (MonoFoldable,Element,oelem)

(∈) ∷ (MonoFoldable mono, Eq (Element mono)) ⇒ Element mono → mono → Bool
(∈) = oelem
{-# INLINE (∈) #-}

(⊕) ∷ Semigroup α ⇒ α → α → α
(⊕) = (<>)
{-# INLINE (⊕) #-}
