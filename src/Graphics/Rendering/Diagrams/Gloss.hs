module Graphics.Rendering.Diagrams.Gloss where

import qualified Graphics.Gloss.Data.Picture     as G
import qualified Graphics.Rendering.Diagrams.AST as D

{- Gloss:

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

fromGloss :: G.Picture -> D.Picture
fromGloss = undefined

toGloss :: D.Picture -> G.Picture
toGloss = undefined
