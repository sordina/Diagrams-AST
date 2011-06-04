import Graphics.Rendering.Diagrams.AST
import System.Random
import Control.Applicative

main = do
  heights <- fmap (sort . take 100) $ (zipWith (+)) <$> range (0,10) <*> range (0,10)
  outputImage ("chart.png") 800 600 $ barchart heights

barchart = Modifier (Changes [Align C, Pad 1.2]) . Images . Horizontal . map bar

bar y = Modifier (Changes [Align B, Foreground white, Scale 1 (5*y)]) (Shape Square)

range b = fmap (randomRs b) newStdGen

white = RGBA 1 1 1 1
