module Types.Tetris (
gridSize,
initialGrid,
initialWindow,
initialGame,
getWindowSize,
getGameGrid,
valueInGridAt,
Grid(),
Game()
)
where

import Control.Applicative ((<$>))
import Control.Applicative ((<*>))

getGameGrid :: Game -> Grid
getGameGrid = game_grid

data  Game = Game { game_grid :: Grid, window :: Window }

newtype Grid = MkGrid {getGrid :: [[Bool]]} 
newtype Window = MkWindow {game_size :: (Int, Int)}

gridSize :: Grid -> (Int , Int)
gridSize = ((,) <$> length . head <*> length) . getGrid

getWindowSize :: Game -> (Int, Int)
getWindowSize = game_size . window 

initialGame :: Game
initialGame = Game initialGrid initialWindow

initialWindow :: Window
initialWindow = MkWindow (800, 600)

initialGrid :: Grid
initialGrid = MkGrid $ replicate 100 $ replicate 100 True

valueInGridAt :: Grid -> Int -> Int -> Bool
valueInGridAt g x y = getGrid g !! y !! x
