import qualified Diagrams.AST as D
import qualified Diagrams.Gloss as G
import Graphics.Gloss

image = Color red $ Circle 80
diagram = G.fromGloss image

width = 500
height = 300

main = do
  print diagram
  D.outputImage "gloss_to_diagram.png" width height diagram
  displayInWindow "Gloss -> Diagrams Test" (width, height) (10, 10) white image
