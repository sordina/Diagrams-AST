{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Graphics.Rendering.Diagrams.AST (
  -- Functions
  outputImage,
  runImage,

  -- Data Types
  Image     (..),
  Modifier  (..),
  Combiner  (..),
  Path      (..),
  Shape     (..),
  ColorData (..),
  Fill      (..),
  NodeStyle (..),

  -- Newtypes
  Offset (..),
  Point  (..),
  Angle  (..)
  )
where

-- Diagram Imports
import qualified Diagrams.Prelude as D
import           Diagrams.Prelude ((|||), (===))
import           Diagrams.Backend.Cairo

-- Data Imports
import Data.Monoid
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
          | Points  [Point]
          | Arc      Angle  deriving (Show, Eq, Ord)

data Shape = Circle
           | Path Fill NodeStyle Path deriving (Show, Eq, Ord)

data ColorData = ColorData Double Double Double Double deriving (Show, Eq, Ord)

data Fill = Joined
          | Open deriving (Show, Eq, Ord)

data NodeStyle = Smooth
               | Sharp deriving (Show, Eq, Ord)

newtype Offset = Offset Double deriving (Show, Eq, Ord)
newtype Point  = Point  Double deriving (Show, Eq, Ord)
newtype Angle  = Angle  Double deriving (Show, Eq, Ord)

--- Instances

instance D.Color ColorData where
  colorToRGBA (ColorData r g b a) = (r, g, b, a)

---- Run ADT Functions

outputImage name width height image = D.renderDia Cairo (CairoOptions name (PNG (width, height))) (runImage image)

--- Main runner

runImage (Shape s)      = runShape s
runImage (Modifier m i) = runModifier m (runImage i)
runImage (Combiner c)   = runCombiner c

--- Internal runners

runCombiner (Atop   l r)   = runImage l `D.atop` runImage r
runCombiner (NextTo l r)   = runImage l ||| runImage r
runCombiner (Above  t b)   = runImage t === runImage b
runCombiner (Layers     l) = mconcat . map runImage $ l
runCombiner (Horizontal l) = D.hcat (map runImage l)
runCombiner (Vertical   l) = D.vcat (map runImage l)

runShape Circle = D.circle

runModifier (Foreground c)  = D.fillColor c
runModifier (LineColor  c)  = D.lineColor c
runModifier (LineWidth  w)  = D.lw w
runModifier (Dashing  a w)  = D.dashing a w
runModifier (Translate x y) = D.translate (x, y)
runModifier (Scale x y)     = D.scaleX x . D.scaleY y
runModifier (Pad r)         = D.pad r
runModifier (Changes l)     = foldl' (.) id . map runModifier $ l
runModifier  Freeze         = D.freeze
