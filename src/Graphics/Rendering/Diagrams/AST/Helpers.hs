module Graphics.Rendering.Diagrams.AST.Helpers ( barchart) where

import Graphics.Rendering.Diagrams.AST

barchart :: ColorData -> [Double] -> Image
barchart color = Modifier (Changes [Align C, Pad 1.2]) . Images . Horizontal . map bar
  where
    bar y = Modifier (Changes [Align B, Foreground color, Scale 1 (5*y)]) (Shape Square)
