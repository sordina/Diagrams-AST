module Graphics.Rendering.Diagrams.AST.Optimize ( optimize ) where

import Graphics.Rendering.Diagrams.AST
import Data.Generics.Uniplate.Operations
import Data.List (sort)

-- Consider using "rewrite" instead of transform (http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm)
optimize :: Image -> Image
optimize = transform o

-- Don't modify blank images
o (Modifier _ Blank) = Blank

-- Leaving the debate about pattern-matching on doubles until later
o (Modifier (Scale 0 _) _) = Blank
o (Modifier (Scale _ 0) _) = Blank
o (Modifier (Scale 1 1) i) = i

-- Consecutive Scales
o (Modifier (Scale x y) (Modifier (Scale x' y') i)) = Modifier (Scale (x*x') (y*y')) i

-- Consecutive Translations
o (Modifier (Translate x y) (Modifier (Translate x' y') i)) = Modifier (Translate (x*x') (y*y')) i

o (Modifier (Changes [])  i) = i
o (Modifier (Changes [c]) i) = Modifier c i
-- o (Modifier (Changes l)   i) = Modifier (Changes (sort l)) i -- Not concatinating yet

-- Removing Blanks from Combinations
-- Sort lists to allow for transformation concatinations
o (Images (Atop Blank i))   = i
o (Images (Atop  i Blank))  = i
o (Images (Above Blank i))  = i
o (Images (Above i Blank))  = i
o (Images (Layers     []))  = Blank
o (Images (Layers     [i])) = i
o (Images (Layers     xs))  = Images $ Layers $ filter (not.blank) (sort xs)
o (Images (Vertical   []))  = Blank
o (Images (Vertical   [i])) = i
o (Images (Vertical   xs))  = Images $ Vertical $ filter (not.blank) (sort xs)
o (Images (Horizontal []))  = Blank
o (Images (Horizontal [i])) = i
o (Images (Horizontal xs))  = Images $ Horizontal $ filter (not.blank) (sort xs)

-- etc, etc

-- Default
o x = x

-- Helpers
blank :: Image -> Bool
blank Blank = True
blank _     = False
