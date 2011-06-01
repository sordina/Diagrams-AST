import Graphics.Rendering.Diagrams.AST
import System.Random

main = do
  ps <- paths
  outputImage "../angles.png" 600 600 $
    Images $
      Layers ps

paths :: IO [Image]
paths = do
  r1 <- range
  r2 <- range
  return $ map path $ take 100 $ chunk 3 $ zip r1 r2

path l =
  Modifier (Foreground white) $
  Shape $
  Path Joined Sharp $
  Offsets $
  offsets l

offsets = map Offset

white = ColorData 1 1 1 1

chunk :: Int -> [x] -> [[x]]
chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

range :: IO [Double]
range = do
  g <- newStdGen
  return $ randomRs (-10,10) g
