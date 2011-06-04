import Graphics.Rendering.Diagrams.AST

main = outputImage "../test.png" 300 300 (Modifier (Pad 3) boob)

boobs  = Combiner (Horizontal [boob, boob])
boob   = Modifier (Pad 1) $ Combiner (Atop nipple breast)
breast = Modifier (Foreground pink) circle
nipple = Modifier (Changes [Scale 0.1 0.1, Foreground brown]) circle
pink   = ColorData 1   0.8  0.8 1
brown  = ColorData 0.9 0.5  0.1 1
circle = Shape Circle
