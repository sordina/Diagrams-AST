{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Graphics.Rendering.Diagrams.AST (
  -- Functions
  outputImage,
  runImage,

  -- Data Types
  Image     (..),
  Modifier  (..),
  Images    (..),
  Path      (..),
  Shape     (..),
  ColorData (..),
  Fill      (..),
  NodeStyle (..),

  -- Newtypes
  Angle  (..)
  )
where

-- Diagram Imports
import qualified Diagrams.Prelude                   as D
import qualified Diagrams.Path                      as P
import qualified Diagrams.TwoD.Path                 as P2
import qualified Diagrams.TwoD.Arc                  as A
import qualified Diagrams.Backend.Cairo             as C
import qualified Graphics.Rendering.Diagrams.Points as P3

import           Diagrams.Prelude ((|||), (===))

-- Data Imports
import Data.Monoid
import Data.List (foldl')

--- Data Types

data Image = Shape Shape
           | Modifier Modifier Image
           | Images Images deriving (Show, Eq, Ord)

data Modifier = Foreground       ColorData
              | LineColor        ColorData
              | LineWidth        Double
              | Dashing [Double] Double
              | Translate Double Double
              | Scale Double Double
              | Pad Double
              | Freeze
              | Changes [Modifier] deriving (Show, Eq, Ord)

data Images = Atop   Image Image
            | NextTo Image Image
            | Above  Image Image
            | Layers     [Image]
            | Horizontal [Image]
            | Vertical   [Image] deriving (Show, Eq, Ord)

data Shape = Circle
           | Path Fill NodeStyle Path deriving (Show, Eq, Ord)

data Path = Offsets [(Double,Double)]
          | Points  [(Double,Double)]
          | Arc Angle Angle deriving (Show, Eq, Ord)

data ColorData = ColorData Double Double Double Double deriving (Show, Eq, Ord)

data Fill = Closed
          | Open deriving (Show, Eq, Ord)

data NodeStyle = Smooth
               | Sharp deriving (Show, Eq, Ord)

newtype Angle  = Radians { getAngle  ::  Double }          deriving (Show, Eq, Ord)

--- Instances

instance D.Color ColorData where
  colorToRGBA (ColorData r g b a) = (r, g, b, a)

---- Run ADT Functions

outputImage name width height image =
  D.renderDia C.Cairo (C.CairoOptions name (C.PNG (width, height))) (runImage image)

--- Main runner

runImage (Shape s)      = runShape s
runImage (Modifier m i) = runModifier m (runImage i)
runImage (Images c)   = runCombiner c

--- Internal runners

runCombiner (Atop   l r)   = runImage l `D.atop` runImage r
runCombiner (NextTo l r)   = runImage l ||| runImage r
runCombiner (Above  t b)   = runImage t === runImage b
runCombiner (Layers     l) = mconcat . map runImage $ l
runCombiner (Horizontal l) = D.hcat (map runImage l)
runCombiner (Vertical   l) = D.vcat (map runImage l)

runShape  Circle      = D.circle
runShape (Path Closed n p) = P2.stroke $ P.close $ runPath p
runShape (Path Open   n p) = P2.stroke $ P.open  $ runPath p -- TODO: something with n

runModifier (Foreground c)  = D.fillColor c
runModifier (LineColor  c)  = D.lineColor c
runModifier (LineWidth  w)  = D.lw w
runModifier (Dashing  a w)  = D.dashing a w
runModifier (Translate x y) = D.translate (x, y)
runModifier (Scale x y)     = D.scaleX x . D.scaleY y
runModifier (Pad r)         = D.pad r
runModifier (Changes l)     = foldl' (.) id . map runModifier $ l
runModifier  Freeze         = D.freeze

runPath (Offsets l) = P.fromOffsets l
runPath (Points  l) = (P.fromVertices . map P3.P) l
runPath (Arc b   e) = A.arc (getAngle b) (getAngle e)
