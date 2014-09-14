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
Game(),
moveShape,
Direction(..),
updateGravity
)
where

import Control.Applicative ((<$>), (<*>))
import Control.Lens ((&) , (%~), _Just, (.~), (^.), (^?), makeLenses, view)
import Data.List as L
import Data.Maybe (fromMaybe)

type Grid = [[Bool]]
newtype Window = MkWindow {gameSize :: (Int, Int)}
newtype TetrisShape = MkShape {_blocks :: [(Int,Int)]} deriving Eq
data  Game = Game { _grid :: Grid,  _window :: Window, _currentShape :: Maybe TetrisShape }

makeLenses ''TetrisShape
makeLenses ''Game

data Direction = DLeft | DRight | DDown | DUp


getGameGrid :: Game -> Grid
getGameGrid = _grid

gridSize :: Grid -> (Int , Int)
gridSize = (,) <$> length . head <*> length 

getWindowSize :: Game -> (Int, Int)
getWindowSize = gameSize . view window 

initialGame :: Game
initialGame = Game g initialWindow s
              where (s, g) = getNextShape (initialGrid)

getNextShape :: Grid -> (Maybe TetrisShape , Grid)
getNextShape g = (Just $ MkShape s , ng)
                 where 
                   (w,h) = gridSize g
                   s' = head possibleShapes
                   f (x, y) = ((w `div` 2) - x, h - 1 - y)
                   s = map f s'
                   ng = addShapeToGrid s g

possibleShapes :: [[(Int, Int)]]
possibleShapes = [[(0,0) , (0,1) , (1, 1), (1,0)]]

initialWindow :: Window
initialWindow = MkWindow (800, 600)

initialGrid :: Grid
initialGrid =  replicate 10 $ replicate 10 False

valueInGridAt :: Grid -> Int -> Int -> Bool
valueInGridAt g x y =  g !! y !! x

--yuck
setGridAt :: Grid -> Int -> Int -> Bool -> Grid
setGridAt g x y v = update y row g
                    where row = update x (const v)

update :: Eq a => Int -> (a -> a) -> [a] -> [a] 
update i f xs = map repl $ zip xs [0..]
    where repl (a, i')  | i == i' = f a
                        | otherwise = a

moveShape :: Direction -> Game -> Game
moveShape d g =  if isValidPosition (g ^. grid) oldShape newShape 
                 then updateGame g oldShape newShape 
                 else g
             where 
               oldShape = fromMaybe [] $ g ^? currentShape . _Just . blocks
               newShape = moveShapePoints d oldShape

updateGame :: Game -> [(Int,Int)] -> [(Int, Int)] -> Game
updateGame g old new = g 
                       & grid .~ newGrid 
                       & currentShape .~ (Just . MkShape) new

    where
      newGrid = addShapeToGrid new $ removeShapeFromGrid old $ g ^. grid

removeShapeFromGrid :: [(Int,Int)] -> Grid -> Grid
removeShapeFromGrid = updateGridWithShape False

addShapeToGrid :: [(Int,Int)] -> Grid -> Grid
addShapeToGrid = updateGridWithShape True

updateGridWithShape :: Bool ->  [(Int,Int)] -> Grid -> Grid
updateGridWithShape b s g = foldr f g s
    where f (x, y) g' = setGridAt g' x y b
 
isValidPosition :: Grid -> [(Int, Int)] -> [(Int, Int)]->  Bool
isValidPosition g old new = not (outOfBounds || clashingBlock)
    where newPoints = filter (`notElem` old) new
          withinGrid (x , y) = let (x', y') = gridSize g in
                                 x < 0 || y < 0 || x >= x' || y >= y'
          clashingBlock = blockFilled g newPoints
          outOfBounds = L.any withinGrid newPoints

blockFilled :: Grid -> [(Int, Int)] -> Bool
blockFilled g =  L.or . map (uncurry (valueInGridAt g))


moveShapePoints :: Direction -> [(Int, Int)] -> [(Int, Int)]
moveShapePoints d =  map $ directionToVector d 

directionToVector :: (Num t1, Num t) => Direction -> (t, t1) -> (t, t1)
directionToVector d (x, y)= case d of 
                        DLeft -> (x-1, y)
                        DRight -> (x+1, y)
                        DDown -> (x, y-1)
                        DUp -> (x, y +1)

updateGravity :: Game -> Game
updateGravity g = if oldShape == newShape then shapePlaced newGame else newGame
                  where oldShape = g ^. currentShape
                        newGame = moveShape DDown g
                        newShape = newGame ^. currentShape

shapePlaced :: Game -> Game
shapePlaced = addNewShape . detectLoss . removeFullRows

addNewShape :: Game -> Game
addNewShape g = g 
                & currentShape .~ s
                & grid .~ gr
                where (s, gr)  = getNextShape $ g ^. grid

detectLoss :: Game -> Game
detectLoss g = if L.or $ last $ g ^. grid  then error "Game over" else g

removeFullRows :: Game -> Game
removeFullRows g = g & grid .~ newGrid
    where gr = g ^. grid
          (w, h) = gridSize gr
          notFull = not . L.and
          emptyRow = replicate w False
          newGrid = take h (filter notFull gr ++ repeat emptyRow)
