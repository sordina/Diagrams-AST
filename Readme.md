Diagrams AST (Abstract Syntax Tree)
===================================

<img src="/sordina/Diagrams-AST/raw/master/documentation/chart.png" alt="Bar-Chart Example" />

Diagrams AST provides a data oriented interface to the Graphics.Rendering.Diagrams package.

This can be useful as it allows inspection and modification of diagrams.

## Example:

	import Graphics.Rendering.Diagrams.AST
	import System.Random
	import Control.Applicative
	import Data.List (sort)
	
	main = do
	  heights <- fmap (sort . take 100) $ (zipWith (+)) <$> range (0,10) <*> range (0,10)
	  outputImage ("chart.png") 400 300 $ barchart heights
	
	barchart = Modifier (Changes [Align C, Pad 1.2]) . Images . Horizontal . map bar
	
	bar y = Modifier (Changes [Align B, Foreground blue, Scale 1 (5*y)]) (Shape Square)
	
	range b = fmap (randomRs b) newStdGen
	
	blue = RGBA 0.2 0.3 1 1


## To-do:

* Move rendering code to a separate package
* Move Gloss dependency to separate package
