{-# LANGUAGE TemplateHaskell #-}

module Data.Tetris (
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

import Control.Applicative ((<$>), (<*>))
import Control.Lens ((&) , (%~), _Just, (.~), (^.), (^?), makeLenses, view)
import Data.List as L
import Data.Maybe (fromMaybe)

type Grid = [[Bool]]
newtype Window = MkWindow {gameSize :: (Int, Int)}
newtype TetrisShape = MkShape {_blocks :: [(Int,Int)]}
data  Game = Game { _grid :: Grid,  _window :: Window, _currentShape :: Maybe TetrisShape }

makeLenses ''TetrisShape
makeLenses ''Game

data Direction = DLeft | DRight | DDown


getGameGrid :: Game -> Grid
getGameGrid = _grid

gridSize :: Grid -> (Int , Int)
gridSize = (,) <$> length . head <*> length 

getWindowSize :: Game -> (Int, Int)
getWindowSize = gameSize . view window 

initialGame :: Game
initialGame = Game initialGrid initialWindow Nothing

initialWindow :: Window
initialWindow = MkWindow (800, 600)

initialGrid :: Grid
initialGrid =  replicate 100 $ concat $ replicate 50 [True, False]

valueInGridAt :: Grid -> Int -> Int -> Bool
valueInGridAt g x y =  g !! y !! x

moveShape :: Direction -> Game -> Game
moveShape d g =  if isValidPosition (g ^. grid) oldShape newShape then g else g
             where 
               oldShape = fromMaybe [] $ g ^? currentShape . _Just . blocks
               newShape = moveShapePoints d oldShape

isValidPosition :: Grid -> [(Int, Int)] -> [(Int, Int)]->  Bool
isValidPosition grid old new = not (outOfBounds || clashingBlock)
    where newPoints = filter (`notElem` old) new
          withinGrid g (x , y) = let (x', y') = gridSize g in
                                 x < 0 || y < 0 || x >= x' || y >= y'
          clashingBlock = blockFilled grid newPoints
          outOfBounds = L.any (withinGrid grid) newPoints

blockFilled :: Grid -> [(Int, Int)] -> Bool
blockFilled grid =  L.or . map (uncurry (valueInGridAt grid))


moveShapePoints :: Direction -> [(Int, Int)] -> [(Int, Int)]
moveShapePoints d =  map $ directionToVector d 

directionToVector :: (Num t1, Num t) => Direction -> (t, t1) -> (t, t1)
directionToVector d (x, y)= case d of 
                        DLeft -> (x-1, y)
                        DRight -> (x+1, y)
                        DDown -> (x, y-1)

