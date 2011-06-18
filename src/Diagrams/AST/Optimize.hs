{-# OPTIONS -fno-warn-overlapping-patterns #-}

-- | Optimizations to Image data-structures. Takes advantage of <http://hackage.haskell.org/package/uniplate>.

module Diagrams.AST.Optimize ( optimize ) where

import Diagrams.AST
import Data.Generics.Uniplate.Data

{- | The function 'optimize' takes an Image and returns an equivilant Image that optimizes the structure of the data.

     Optimizations include:

     * Turning @ 0 @ scalings into 'Blank' images

     * Concatinating consecutive scalings, rotations, and translations

     * Removing identity transformations

     * Turning singleton transformation lists into regular transformations
-}

-- Using "rewrite" instead of transform (http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm)
optimize :: Image -> Image
optimize = reList . rewrite o . deList

-- Don't modify blank images
o (Modifier _ Blank) = Just Blank

-- Leaving the debate about pattern-matching on doubles until later

-- Identity scaling
o (Modifier (Scale 0 _) _) = Just Blank
o (Modifier (Scale _ 0) _) = Just Blank
o (Modifier (Scale 1 1) i) = Just i

-- Rotate a Circle? WTF?
o (Modifier (Rotate _) (Shape Circle)) = Just $ Shape Circle

-- Identity rotations - Monoidal properties on angle
o (Modifier (Rotate r') i)
  | norm r' == 0     = Just i
  | r' > 1 || r' < 0 = Just $ Modifier (Rotate $ norm r') i
  | otherwise        = Nothing
  where
    norm r
      | r > 1     = norm (r-1)
      | r < 0     = norm (r+1)
      | otherwise = r

-- Identity translations
o (Modifier (Translate 0 0) i) = Just i

-- Consecutive Scales
o (Modifier (Scale x y) (Modifier (Scale x' y') i)) = Just $ Modifier (Scale (x*x') (y*y')) i

-- Consecutive Translations
o (Modifier (Translate x y) (Modifier (Translate x' y') i)) = Just $ Modifier (Translate (x+x') (y+y')) i

-- Consecutive Rotations
o (Modifier (Rotate a) (Modifier (Rotate a') i)) = Just $ Modifier (Rotate (a+a')) i

-- Removing Blanks from Combinations
o (Images (Atop Blank i))    = Just i
o (Images (Atop  i Blank))   = Just i
o (Images (Above Blank i))   = Just i
o (Images (Above i Blank))   = Just i
o (Images (NextTo Blank i))  = Just i
o (Images (NextTo i Blank))  = Just i

-- Default
o _ = Nothing

-- Helpers

deList :: Image -> Image
deList = transform d
  where
    -- Changes
    d (Modifier (Changes []) i) = i
    d (Modifier (Changes l)  i) = (foldl1 (.) . map Modifier $ l) i
    -- Layers
    d (Images (Layers [])) = Blank
    d (Images (Layers l))  = f Atop l
    -- Vertical
    d (Images (Vertical [])) = Blank
    d (Images (Vertical l))  = f Above l
    -- Horizontal
    d (Images (Horizontal [])) = Blank
    d (Images (Horizontal l))  = f NextTo l
    -- Default
    d i = i
    -- Helpers
    f c = foldr (Images `owl` c) Blank where owl = (.).(.)

reList :: Image -> Image
reList = rewrite r
  where
    -- Modifications
    r (Modifier m (Modifier (Changes l) i)) = Just $ Modifier (Changes (m:l))   i
    r (Modifier m (Modifier m'          i)) = Just $ Modifier (Changes [m, m']) i
    -- Atop
    r (Images (Atop i (Images (Layers l)))) = Just $ Images $ Layers (i:l)
    r (Images (Atop i (Images (Atop j k)))) = Just $ Images $ Layers [i, j, k]
    -- Above
    r (Images (Above i (Images (Vertical l)))) = Just $ Images $ Vertical (i:l)
    r (Images (Above i (Images (Above j k))))  = Just $ Images $ Vertical [i, j, k]
    -- NextTo
    r (Images (NextTo i (Images (Horizontal l)))) = Just $ Images $ Horizontal (i:l)
    r (Images (NextTo i (Images (NextTo j k))))   = Just $ Images $ Horizontal [i, j, k]
    -- Default
    r _ = Nothing
