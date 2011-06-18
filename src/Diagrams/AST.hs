{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, DeriveDataTypeable #-}

-- | Diagrams-AST provides a data oriented interface to the <http://hackage.haskell.org/package/diagrams> package.

module Graphics.Rendering.Diagrams.AST (
  -- Functions
  outputImage,
  runImage,
  getAngleFraction,
  getAngleRadians,
  getAngleDegrees,

  -- Data Types
  Image     (..),
  Modifier  (..),
  Images    (..),
  Path      (..),
  Shape     (..),
  ColorData (..),
  Fill      (..),
  Alignment (..),
  Angle     (..)
  )
where

-- Diagram Imports
import qualified Diagrams.Prelude                   as D
import qualified Diagrams.Path                      as P
import qualified Diagrams.TwoD.Path                 as P2
import qualified Diagrams.TwoD.Arc                  as A
import qualified Diagrams.TwoD.Align                as L
import qualified Diagrams.Backend.Cairo             as C
import qualified Graphics.Rendering.Diagrams.Points as P3

import Diagrams.Prelude ((|||), (===))

-- Data Imports
import Data.Monoid
import Data.List (foldl')

-- Meta
import Data.Generics.Uniplate.Data
import Data.Data

--- Data Types

data Image = Blank
           | Shape Shape
           | Modifier Modifier Image
           | Images Images deriving (Show, Eq, Ord, Data, Typeable)

data Modifier = Foreground       ColorData
              | LineColor        ColorData
              | LineWidth        Double
              | Dashing [Double] Double
              | Translate Double Double
              | Scale Double Double
              | Rotate Angle
              | Pad Double
              | Freeze
              | Origin
              | Align Alignment
              | Changes [Modifier] deriving (Show, Eq, Ord, Data, Typeable)

data Images = Atop   Image Image
            | NextTo Image Image
            | Above  Image Image
            | Layers     [Image]
            | Horizontal [Image]
            | Vertical   [Image] deriving (Show, Eq, Ord, Data, Typeable)

data Shape = Circle | Square | Path Fill Path deriving (Show, Eq, Ord, Data, Typeable)

data Path = Offsets [(Double,Double)]
          | Points  [(Double,Double)]
          | Arc Angle Angle deriving (Show, Eq, Ord, Data, Typeable)

data ColorData = RGBA Double Double Double Double -- ^ Red, Green, Blue, Alpha
               | RAA  Double Double Double Double -- ^ Radius, Blue\/Green, (Blue\/Green)\/Red
               deriving (Show, Eq, Ord, Data, Typeable)

data Fill = Closed | Open deriving (Show, Eq, Ord, Data, Typeable)

-- | Alignment of the origin of an 'Image'.
data Alignment = L        -- ^ Left
               | R        -- ^ Right
               | T        -- ^ Top
               | B        -- ^ Bottom
               | TL       -- ^ Top-Left
               | TR       -- ^ Top-Right
               | BL       -- ^ Bottom-Left
               | BR       -- ^ Bottom-Right
               | C        -- ^ Center
               | CX       -- ^ X-Centered
               | CY       -- ^ Y-Centered
               | X Double -- ^ X-Proportion (Fraction -1 to 1)
               | Y Double -- ^ Y-Proportion (Fraction -1 to 1)
               deriving (Show, Eq, Ord, Data, Typeable)

-- | Angles are instances of Num. 'fromInteger' interprets its argument as a fraction of a full circle.
data Angle = Fraction Double | Radians Double | Degrees Double deriving (Show, Eq, Ord, Data, Typeable)

instance Num Angle
  where
    fromInteger x = Fraction $ fromInteger x
    x + y = Fraction $ (getAngleFraction x) + (getAngleFraction y)
    x * y = Fraction $ (getAngleFraction x) * (getAngleFraction y)
    abs x = Fraction $ abs (getAngleFraction x)
    signum x
      | x' > 0 = 1
      | x' < 0 = -1
      | otherwise = 0
      where x' = getAngleFraction x

-- | 'getAngleFraction' returns the fraction of a full circle for any angle.
getAngleFraction :: Angle -> Double
getAngleFraction (Fraction x) = x
getAngleFraction (Radians  x) = x / (2*pi)
getAngleFraction (Degrees  x) = x / 360
getAngleRadians = (* 2) . (* pi) . getAngleFraction
getAngleDegrees = (* 360)        . getAngleFraction

--- Instances

instance D.Color ColorData where
  colorToRGBA (RGBA r g b a) = (r, g, b, a)
  colorToRGBA (RAA  r g e a) = ( r * cos g * cos e,
                                 r * cos g * sin e,
                                 r * sin g, a )

---- Run ADT Functions

-- | 'outputImage' renders a PNG to the file supplied.
outputImage :: String -> Int -> Int -> Image -> IO ()
outputImage name width height image =
  D.renderDia C.Cairo (C.CairoOptions name (C.PNG (width, height))) (runImage image)

--- Main runner

-- 'runImage' creates a "Graphics.Rendering.Diagrams" image from a "Graphics.Rendering.Diagrams.AST" image.
runImage (Shape s)      = runShape s
runImage (Modifier m i) = runModifier m (runImage i)
runImage (Images c)     = runCombiner c
runImage Blank          = mempty

--- Internal runners

runCombiner (Atop   l r)   = runImage l `D.atop` runImage r
runCombiner (NextTo l r)   = runImage l ||| runImage r
runCombiner (Above  t b)   = runImage t === runImage b
runCombiner (Layers     l) = mconcat . map runImage $ l
runCombiner (Horizontal l) = D.hcat (map runImage l)
runCombiner (Vertical   l) = D.vcat (map runImage l)

runShape  Circle         = D.circle
runShape  Square         = D.square
runShape (Path Closed p) = P2.stroke $ P.close $ runPath p
runShape (Path Open   p) = P2.stroke $ P.open  $ runPath p

runModifier (Foreground c)  = D.fillColor c
runModifier (LineColor  c)  = D.lineColor c
runModifier (LineWidth  w)  = D.lw w
runModifier (Dashing  a w)  = D.dashing a w
runModifier (Translate x y) = D.translate (x, y)
runModifier (Rotate a)      = D.rotateBy (getAngleFraction a)
runModifier (Scale x y)     = D.scaleX x . D.scaleY y
runModifier (Pad r)         = D.pad r
runModifier (Align a)       = runAlign a
runModifier (Changes l)     = foldl' (flip (.)) id . map runModifier $ l
runModifier  Origin         = D.showOrigin
runModifier  Freeze         = D.freeze

runPath (Offsets l) = P.fromOffsets l
runPath (Points  l) = (P.fromVertices . map P3.P) l
runPath (Arc b   e) = A.arc (getAngleRadians b) (getAngleRadians e)

runAlign L     = L.alignL
runAlign R     = L.alignR
runAlign T     = L.alignT
runAlign B     = L.alignB
runAlign TL    = L.alignTL
runAlign TR    = L.alignTR
runAlign BL    = L.alignBL
runAlign BR    = L.alignBR
runAlign C     = L.centerXY
runAlign CX    = L.centerX
runAlign CY    = L.centerY
runAlign (X x) = L.alignX $ toRational x
runAlign (Y y) = L.alignY $ toRational y