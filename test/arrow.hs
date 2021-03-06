import Diagrams.AST
import Control.Arrow

main = outputImage "arrow.png" 1366 768 $ Images $ Horizontal $ map h [0..4]

h x = Images $ Vertical $ map (v x) [0..4]

v x y = arrow (x/5) (y/5)

arrow g e = Modifier (Changes [LineWidth 0.1, Foreground (RAA 1 (Radians g) (Radians e) 0.94), Align CX, Pad 1.01]) $
    Shape $
    Path Closed $ Offsets $ forwards ++ backwards

forwards  = [ (0,1), (5,0), (0,1), (3,-2) ]
backwards = map (first negate) (reverse forwards)
