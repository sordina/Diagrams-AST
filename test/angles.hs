import Graphics.Rendering.Diagrams.AST
import System.Random

main = mapM_ step [1..100]

step n = do
  print n
  ps <- paths
  outputImage ("../angles_" ++ show n ++ ".png") 600 600 $
    Modifier (Pad 1.1) $ Images $ Layers (circle : ps)

paths :: IO [Image]
paths = do
  r1 <- range (-5,5)
  r2 <- range (-5,5)
  c1 <- range  (0,1)
  c2 <- range  (0,1)
  return $ zipWith shape
    (zipWith color c1 c2)
    (take 100 $ chunk 5 $ zip r1 r2)

shape c l = Modifier (Changes [Foreground c, LineWidth 0.05]) $
            Shape $
            Path Closed $
            Offsets l

chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

range b = do
  g <- newStdGen
  return $ randomRs b g

color g e = RAA 1 g e 0.94

circle = Modifier (Scale 30 30) $ Shape Circle
