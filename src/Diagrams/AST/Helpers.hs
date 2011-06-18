module Diagrams.AST.Helpers ( barchart, linechart ) where

import Diagrams.AST

barchart :: ColorData -> [Double] -> Image
barchart color = Images . Horizontal . map bar
  where
    bar y = Modifier (Changes [Align B, Foreground color, Scale 1 y]) (Shape Square)

linechart :: [Double] -> Image
linechart [] = Blank
linechart l  = Modifier (uncurry Translate h) . Shape . Path Open . Offsets $ d
  where
    h  = (0, head l)
    dy = zipWith (-) (tail l) l
    d  = zip (repeat 1) dy
