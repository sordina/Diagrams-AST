import Diagrams.AST

main     = outputImage "recursion.png" 300 300 pilla
pilla    = Images $ Atop face body
face     = Modifier (Scale 0.3 0.3) $ Images $ Horizontal [pcirc, pcirc]
pcirc    = Modifier (Foreground black) $ Shape Circle
body     = circles !! 50
circles  = iterate (Images . Atop circle . modifier) Blank
circle   = Modifier (Foreground green) $ Shape Circle
modifier = Modifier $ Changes [Scale 0.9 0.9, Rotate (Fraction 0.1), Translate 0 (-2)]
green    = RGBA 0.1 0.9 0.1 1
black    = RGBA 0 0 0 1
