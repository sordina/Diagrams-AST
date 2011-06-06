module Graphics.Rendering.Diagrams.AST.Optimize ( optimize) where

import Graphics.Rendering.Diagrams.AST
import Data.Generics.Uniplate.Operations


optimize :: Image -> Image
optimize = transform o

-- Don't modify blank images
o (Modifier _ Blank) = Blank

-- Leaving the debate about pattern-matching on doubles until later
o (Modifier (Scale 0 _) _) = Blank
o (Modifier (Scale _ 0) _) = Blank
o (Modifier (Scale 1 1) i) = i

-- Consecutive Scales
o (Modifier (Scale x y) (Modifier (Scale x' y') i)) =
  Modifier (Scale (x*x') (y*y')) i

-- Consecutive Translations
o (Modifier (Translate x y) (Modifier (Translate x' y') i)) =
  Modifier (Translate (x*x') (y*y')) i

-- Removing Blanks from Combinations
o (Images x) = f x
  where
    f (Atop  Blank i)     = i
    f (Atop  i     Blank) = i
    f (Above Blank i)     = i
    f (Above i     Blank) = i
    f (Layers     []) = Blank
    f (Horizontal []) = Blank
    f (Vertical   []) = Blank
-- etc, etc

-- Default
o x = x
