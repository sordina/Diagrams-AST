module Graphics.Rendering.Diagrams.AST.Optimize ( optimize ) where

import Graphics.Rendering.Diagrams.AST
import Data.Generics.Uniplate.Data

-- Using "rewrite" instead of transform (http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm)
optimize :: Image -> Image
optimize = rewrite o

-- Don't modify blank images
o (Modifier _ Blank) = Just $ Blank

-- Leaving the debate about pattern-matching on doubles until later
o (Modifier (Scale 0 _) _) = Just $ Blank
o (Modifier (Scale _ 0) _) = Just $ Blank
o (Modifier (Scale 1 1) i) = Just $ i

-- Consecutive Scales
o (Modifier (Scale x y) (Modifier (Scale x' y') i)) = Just $ Modifier (Scale (x*x') (y*y')) i

-- Consecutive Translations
o (Modifier (Translate x y) (Modifier (Translate x' y') i)) = Just $ Modifier (Translate (x+x') (y+y')) i

-- Consecutive Rotations
o (Modifier (Rotate a) (Modifier (Rotate a') i)) = Just $ Modifier (Rotate (a+a')) i

-- Sets of changes
o (Modifier (Changes [])  i) = Just $ i
o (Modifier (Changes [c]) i) = Just $ Modifier c i
o (Modifier (Changes l)   i) = g (f (Left l))
  where
    g (Right l) = Just $ Modifier (Changes l) i
    g (Left  l) = Nothing

    f (Left  (Scale     x y : Scale     x' y' : l)) = f $ Right $ Scale     (x*x') (y*y') : h (f $ Left l)
    f (Left  (Translate x y : Translate x' y' : l)) = f $ Right $ Translate (x+x') (y+y') : h (f $ Left l)
    f (Left  (Rotate    x   : Rotate    x'    : l)) = f $ Right $ Rotate    (x+x')        : h (f $ Left l)
    f x = x

    h (Left  l) = l
    h (Right l) = l

-- Removing Blanks from Combinations
o (Images (Atop Blank i))   = Just $ i
o (Images (Atop  i Blank))  = Just $ i
o (Images (Above Blank i))  = Just $ i
o (Images (Above i Blank))  = Just $ i
o (Images (Layers     []))  = Just $ Blank
o (Images (Layers     [i])) = Just $ i
o (Images (Layers     xs))  = Just $ Images $ Layers $ filter (not.blank) xs
o (Images (Vertical   []))  = Just $ Blank
o (Images (Vertical   [i])) = Just $ i
o (Images (Vertical   xs))  = Just $ Images $ Vertical $ filter (not.blank) xs
o (Images (Horizontal []))  = Just $ Blank
o (Images (Horizontal [i])) = Just $ i
o (Images (Horizontal xs))  = Just $ Images $ Horizontal $ filter (not.blank) xs

-- Default
o x = Nothing

-- Helpers
blank :: Image -> Bool
blank Blank = True
blank _     = False
