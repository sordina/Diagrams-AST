import Graphics.Rendering.Diagrams.AST

main = outputImage "recursion.png" 300 300 (circles 30)

circles :: Int -> Image
circles x
  | x < 0     = Blank
  | otherwise = Images . Above circle . modifier . circles $ x - 1

modifier = Modifier $ Changes [Scale 0.9 0.9, Rotate (Fraction 0.1)]

circle :: Image
circle  = Modifier (Foreground green) $ Shape Circle

green :: ColorData
green = RGBA 0.1 0.9 0.1 1
