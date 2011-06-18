module Graphics.Rendering.Diagrams.Gloss (
  fromGloss,
  toGloss
  ) where

import qualified Graphics.Gloss.Data.Picture as G
import qualified Graphics.Gloss.Data.Color as C
import Graphics.Rendering.Diagrams.AST

{-| Gloss:

data Picture - A 2D picture

Constructors:

Blank
Polygon     Path
Line        Path
Circle      Float
ThickCircle Float Float
Text        String
Bitmap      Int Int ByteString
Color       Color Picture
Translate   Float Float Picture
Rotate      Float Picture
Scale Float Float Picture
Pictures   [Picture]

-}

fromGloss :: G.Picture -> Image
fromGloss (G.Blank)            = Blank
fromGloss (G.Polygon p)        = Shape . Path Closed . Points . map d2 $ p
fromGloss (G.Line p)           = Shape . Path Open . Points . map d2 $ p
fromGloss (G.Circle r)         = Modifier (Scale r' r') (Shape Circle) where r' = d r

-- Missing: ThickCircle, Text, Bitmap

fromGloss (G.Color c p)        = Modifier (Foreground $ color c) (fromGloss p)
fromGloss (G.Translate x y p)  = Modifier (Translate (d x) (d y)) (fromGloss p)
fromGloss (G.Rotate r p)       = Modifier (Rotate $ Degrees (d r)) (fromGloss p)
fromGloss (G.Scale x y p)      = Modifier (Scale (d x) (d y)) (fromGloss p)
fromGloss (G.Pictures l)       = Images . Layers . map fromGloss $ l

toGloss :: Image -> G.Picture
toGloss = undefined

-- Helpers

d :: Float -> Double
d = fromRational . toRational

d2 (x,y) = (d x, d y)

color c = let (r,g,b,a) = C.rgbaOfColor c in RGBA (d r) (d g) (d b) (d a)
