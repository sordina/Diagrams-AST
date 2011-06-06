import Graphics.Rendering.Diagrams.AST
import Graphics.Rendering.Diagrams.AST.Helpers
import System.Random
import Control.Applicative
import Data.List (sort)

main = do
  h1 <- heights
  h2 <- heights
  outputImage "chart.png" 400 300 $ Images $ Atop (Modifier (Changes [LineColor red, LineWidth 0.2]) $ linechart h1) (barchart blue h2)

heights = fmap (sort . take 10) $ zipWith (+) <$> range (0,10) <*> range (0,10)

range b = fmap (randomRs b) newStdGen

blue = RGBA 0.2 0.3 1   1
red  = RGBA 0.8 0.2 0.1 1
