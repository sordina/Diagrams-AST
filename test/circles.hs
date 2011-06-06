import Graphics.Rendering.Diagrams.AST
import Graphics.Rendering.Diagrams.AST.Optimize
import Data.Generics.Uniplate.Operations

main = outputImage "../test.png" 300 300 circles

circles :: Image
circles = Modifier (Rotate $ Fraction 0.1) . Images . Vertical . zipWith Modifier scaling . replicate 10 $ circle

scaling :: [Modifier]
scaling = zipWith Scale xs xs where xs = iterate (* 0.8) 1

circle :: Image
circle  = Modifier (Foreground green) $ Shape Circle

green :: ColorData
green = RGBA 0.1 0.9 0.1 1
