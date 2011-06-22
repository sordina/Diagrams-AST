import Diagrams.AST

main = outputImage "test.png" 3000 3000 $ Modifier (Pad 1.1) $ Images $ Layers $ (take 30 scaledCircles)

circle        = Modifier (Changes [LineWidth (1/20), Freeze]) (Shape Circle)
circles       = repeat   circle
scales        = iterate  (*0.9) 1
scaledCircles = zipWith  combiner scales circles
combiner s c  = Modifier (Changes [LineColor (RAA 1 s 1 s), Scale s s]) c
