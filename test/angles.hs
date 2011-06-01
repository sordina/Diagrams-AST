import Graphics.Rendering.Diagrams.AST
import System.Random

main = do
  ps <- paths
  outputImage "../angles.png" 600 600 $
    Images (Layers ps)

paths :: IO [Image]
paths = do
  r1 <- range (-5,5)
  r2 <- range (-5,5)
  c1 <- range  (0,1)
  c2 <- range  (0,1)
  return $
    map shape $
    zip (zipWith color c1 c2) $
    take 100 $
    chunk 4 $
    zip r1 r2

shape (c,l) =
  Modifier (Changes [Foreground c, LineWidth 0.05]) $
  Shape $
  Path Closed Sharp $
  Offsets l

chunk :: Int -> [x] -> [[x]]
chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

range :: (Double, Double) -> IO [Double]
range b = do
  g <- newStdGen
  return $ randomRs b g

color g e = RAA 1 g e 0.95
