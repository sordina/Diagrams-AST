import Diagrams.AST
import Control.Arrow
import System (getArgs)

main = do
  args <- getArgs
  case args of
       ["-h"] -> print "./arrow <columns> <rows>"
       [x,y]  -> image (read x) (read y)
       _      -> image 1 1

image x y = outputImage "arrow.png" (getX x) (getY y) $ Images $ Horizontal $ map (h x y) [0..x]

h w y x = Images $ Vertical $ flip map [0..y] $ \v -> arrow (2*pi*x/w) (2*pi*v/y)

arrow g e = Modifier (Changes [LineWidth 0, Foreground (RAA 1 g e 0.4), Align CX]) $ Shape $ Path Closed $ Offsets path

forwards  = [ (0,1), (5,0), (0,1), (3,-2) ]
backwards = map (first negate) (reverse forwards)
path      = forwards ++ backwards
width     = (/4) $ sum $ map fst forwards
height    = maximum $ map snd forwards

getY y = floor $ 30 * y * height
getX x = floor $ 30 * x * width
