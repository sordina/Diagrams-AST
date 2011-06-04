import Graphics.Rendering.Diagrams.AST
import System.Random
import Control.Applicative
import Data.List (sort)

main = do
  heights <- fmap (sort . take 100) $ (zipWith (+)) <$> range (0,10) <*> range (0,10)
  outputImage ("chart.png") 800 600 $ barchart heights

barchart = Modifier (Changes [Align C, Pad 1.2]) . Images . Horizontal . map bar

bar y = Modifier (Changes [Align B, Foreground blue, Scale 1 (5*y)]) (Shape Square)

range b = fmap (randomRs b) newStdGen

blue = RGBA 0.2 0.3 1 1
