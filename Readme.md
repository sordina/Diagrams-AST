Diagrams AST (Abstract Syntax Tree)
===================================

<img src="/sordina/Diagrams-AST/raw/master/documentation/chart.png" alt="Bar-Chart Example" />

Diagrams AST provides a data oriented interface to the Graphics.Rendering.Diagrams package.


## Example:

	import Graphics.Rendering.Diagrams.AST
	import System.Random
	import Control.Applicative
	import Data.List (sort)

	main = do 
		heights <- fmap (sort . take 100) $ (zipWith (+)) <$> range (0,10) <*> range (0,10)
		outputImage ("chart.png") 800 600 $ barchart heights

	barchart = Modifier (Changes [Align C, Pad 1.2]) . Images . Horizontal . map bar

	bar y = Modifier (Changes [Align B, Foreground white, Scale 1 (5*y)]) (Shape Square)

	range b = fmap (randomRs b) newStdGen

	white = RGBA 1 1 1 1


## To-do:

Separate out rendering from the AST code (possibly a separate package).
