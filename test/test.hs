import Diagrams.AST


main        = outputImage "text_test.png" 800 400 $ picture

picture     = Images $ Layers $ map mkText [1..15] ++ [bg]
mkText n    = Modifier (Changes [bigger 50, Freeze, Translate 200 0, rotate n, color n]) $ Shape $ Text "clean"
rotate n    = Rotate (Degrees $ n * 24)
color n     = Foreground $ RAA 1 (deg n) (deg n) 1
deg n       = Degrees (n * 24)
red         = Foreground $ RGBA 1 0 0 1
bigger n    = Scale n n
bg          = Modifier (Changes [Foreground $ RGBA 1 1 1 1, Scale 800 800]) $ Shape $ Square
