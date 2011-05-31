{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Graphics.Rendering.Diagrams.AST (
  outputImage,
  runImage,
  Image(..),
  Modifier(..),
  Combiner(..),
  Shape(..),
  ColorData(..)
  )
where

import Diagrams.Prelude hiding (LineColor, LineWidth, Dashing, Point, Path)
import Diagrams.Backend.Cairo
import Data.List (foldl')

--- Data Types

data Image = Shape Shape
           | Modifier Modifier Image
           | Combiner Combiner deriving (Show, Eq, Ord)

data Modifier = Foreground       ColorData
              | LineColor        ColorData
              | LineWidth        Double
              | Dashing [Double] Double
              | Translate Double Double
              | Scale Double Double
              | Pad Double
              | Freeze
              | Changes [Modifier] deriving (Show, Eq, Ord)

data Combiner = Atop   Image Image
              | NextTo Image Image
              | Above  Image Image
              | Layers     [Image]
              | Horizontal [Image]
              | Vertical   [Image] deriving (Show, Eq, Ord)

data Path = Offsets [Offset]
          | Points  [Point] deriving (Show, Eq, Ord)

newtype Offset = Offset Double deriving (Show, Eq, Ord)
newtype Point  = Point  Double deriving (Show, Eq, Ord)

data Shape = Circle
           | Path Fill Path deriving (Show, Eq, Ord)

data ColorData = ColorData Double Double Double Double deriving (Show, Eq, Ord)

data Fill = Join
          | Open deriving (Show, Eq, Ord)

--- Instances

instance Color ColorData where
  colorToRGBA (ColorData r g b a) = (r, g, b, a)

---- Run ADT Functions

outputImage name width height image = renderDia Cairo (CairoOptions name (PNG (width, height))) (runImage image)

--- Main runner

runImage (Shape s)      = runShape s
runImage (Modifier m i) = runModifier m (runImage i)
runImage (Combiner c)   = runCombiner c

--- Internal runners

runCombiner (Atop   l r)   = runImage l `atop` runImage r
runCombiner (NextTo l r)   = runImage l ||| runImage r
runCombiner (Above  t b)   = runImage t === runImage b
runCombiner (Layers     l) = mconcat . map runImage $ l
runCombiner (Horizontal l) = hcat (map runImage l)
runCombiner (Vertical   l) = vcat (map runImage l)

runShape Circle = circle

runModifier (Foreground c)  = fillColor c
runModifier (LineColor  c)  = lineColor c
runModifier (LineWidth  w)  = lw w
runModifier (Dashing  a w)  = dashing a w
runModifier (Translate x y) = translate (x, y)
runModifier (Scale x y)     = scaleX x . scaleY y
runModifier (Pad r)         = pad r
runModifier (Changes l)     = foldl' (.) id . map runModifier $ l
runModifier  Freeze         = freeze
