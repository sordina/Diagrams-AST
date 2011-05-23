{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Graphics.Rendering.Diagrams.AST (
  runImage,
  Image(..),
  Modifier(..),
  Combiner(..),
  Shape(..),
  ColorData(..)
  )
where

import Diagrams.Prelude hiding (LineColor, LineWidth, Dashing)

--- Data Types

data Image = Shape Shape
           | Modifier Modifier Image
           | Combiner Combiner

data Modifier = Foreground       ColorData
              | LineColor        ColorData
              | LineWidth        Double
              | Dashing [Double] Double
              | Translate Double Double
              | Freeze deriving (Show, Eq, Ord)

data Combiner = NextTo Image Image
              | Above  Image Image
              | Horizontal [Image]
              | Vertical   [Image]

data Shape = Circle deriving (Show, Eq, Ord)

data ColorData = ColorData Double Double Double Double deriving (Show, Eq, Ord)

--- Instances

instance Color ColorData where
  colorToRGBA (ColorData r g b a) = (r, g, b, a)

---- Run ADT Functions

--- Main runner

runImage (Shape s)      = runShape s
runImage (Modifier m i) = runModifier m (runImage i)
runImage (Combiner c)   = runCombiner c

--- Internal runners

runCombiner (NextTo l r)   = runImage l ||| runImage r
runCombiner (Above  t b)   = runImage t === runImage b
runCombiner (Horizontal l) = hcat (map runImage l)
runCombiner (Vertical   l) = vcat (map runImage l)

runShape Circle = circle

runModifier (Foreground c)  = fillColor c
runModifier (LineColor  c)  = lineColor c
runModifier (LineWidth  w)  = lw w
runModifier (Dashing  a w)  = dashing a w
runModifier (Translate x y) = translate (x, y)
runModifier  Freeze         = freeze

-- Testing Colors

blue' :: ColorData
blue' = ColorData 0 0 1 1

purple' :: ColorData
purple' = ColorData 1 0 1 1
