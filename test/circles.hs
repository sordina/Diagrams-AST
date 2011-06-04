import Graphics.Rendering.Diagrams.AST

main = outputImage "../test.png" 300 300 circles

circles :: Image
circles = Images . Vertical . zipWith Modifier scaling . replicate 10 $ circle

scaling :: [Modifier]
scaling = map (uncurry Scale) (zip xs xs) where xs = iterate (* 0.8) 1

circle :: Image
circle  = Modifier (Foreground green) $ Shape Circle

green :: ColorData
green = RGBA 0.1 0.9 0.1 1
