
-- | Optimizations to Image data-structures. Takes advantage of <http://hackage.haskell.org/package/uniplate>.

module Graphics.Rendering.Diagrams.AST.Optimize ( optimize ) where

import Graphics.Rendering.Diagrams.AST
import Data.Generics.Uniplate.Data
import Data.Maybe

{- | The function 'optimize' takes an Image and returns an equivilant Image that optimizes the structure of the data.

     Optimizations include:

     * Turning @ 0 @ scalings into 'Blank' images

     * Concatinating consecutive scalings, rotations, and translations

     * Removing identity transformations

     * Turning singleton transformation lists into regular transformations
-}

-- Using "rewrite" instead of transform (http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm)
optimize :: Image -> Image
optimize = rewrite o

-- Don't modify blank images
o (Modifier _ Blank) = Just Blank

-- Leaving the debate about pattern-matching on doubles until later
o (Modifier (Scale 0 _) _) = Just Blank
o (Modifier (Scale _ 0) _) = Just Blank
o (Modifier (Scale 1 1) i) = Just i

o (Modifier (Rotate 0) i) = Just i

o (Modifier (Translate 0 0) i) = Just i

-- Consecutive Scales
o (Modifier (Scale x y) (Modifier (Scale x' y') i)) = Just $ Modifier (Scale (x*x') (y*y')) i

-- Consecutive Translations
o (Modifier (Translate x y) (Modifier (Translate x' y') i)) = Just $ Modifier (Translate (x+x') (y+y')) i

-- Consecutive Rotations
o (Modifier (Rotate a) (Modifier (Rotate a') i)) = Just $ Modifier (Rotate (a+a')) i

-- Rotate a Circle? WTF?
o (Modifier (Rotate a) (Shape Circle)) = Just $ Shape Circle

-- Sets of changes
o (Modifier (Changes [])  i) = Just i
o (Modifier (Changes [c]) i) = Just $ Modifier c i
o (Modifier (Changes l)   i)
  | l == ml   = Nothing
  | otherwise = Just $ Modifier (Changes (deBlank l)) i -- This surely has poor performance
  where
    ml = deBlank l

-- Removing Blanks from Combinations
o (Images (Atop Blank i))   = Just i
o (Images (Atop  i Blank))  = Just i
o (Images (Above Blank i))  = Just i
o (Images (Above i Blank))  = Just i
o (Images (Layers     []))  = Just Blank
o (Images (Layers     [i])) = Just i
o (Images (Vertical   []))  = Just Blank
o (Images (Vertical   [i])) = Just i
o (Images (Horizontal []))  = Just Blank
o (Images (Horizontal [i])) = Just i
o (Images (Horizontal xs))
  | any blank xs = Just $ Images $ Horizontal $ filter (not.blank) xs
  | otherwise    = Nothing
o (Images (Vertical   xs))
  | any blank xs = Just $ Images $ Vertical $ filter (not.blank) xs
  | otherwise    = Nothing
o (Images (Layers     xs))
  | any blank xs = Just $ Images $ Layers $ filter (not.blank) xs
  | otherwise    = Nothing

-- Default
o x = Nothing

-- Helpers
blank :: Image -> Bool
blank Blank = True
blank _     = False

deBlank :: [Modifier] -> [Modifier]
deBlank = rewrite db

db (Rotate    x   : Rotate    x'    : l) = Just (Rotate    (x+x')        : l)
db (Scale     x y : Scale     x' y' : l) = Just (Scale     (x*x') (y*y') : l)
db (Translate x y : Translate x' y' : l) = Just (Translate (x+x') (y+y') : l)
db _ = Nothing
