{-# LANGUAGE TemplateHaskell #-}

module Data.Tetris (
gridSize,
initialWindow,
initialGame,
getWindowSize,
getGameGrid,
valueInGridAt,
Grid(),
Game(),
moveShape,
Direction(..),
Translation(..),
updateGravity
)
where

import Control.Applicative ((<$>), (<*>))
import Control.Lens ((&) , _Just, (.~), (^.), (^?), makeLenses, view, (+~))
import Data.Grid
import Data.List as L
import Data.Maybe (fromMaybe)
import System.Random (RandomGen, newStdGen, randomR)


type GameGrid = Grid Bool

newtype Window = MkWindow {gameSize :: (Int, Int)}
newtype TetrisShape = MkShape {_blocks :: [(Int,Int)]} deriving Eq

data  Game = Game { _grid :: GameGrid,  _window :: Window, _currentShape :: Maybe TetrisShape , _score :: Int}

makeLenses ''TetrisShape
makeLenses ''Game

data Direction = Translation Translation | Rotation
data Translation = DLeft | DRight | DDown | DUp 


getGameGrid :: Game -> GameGrid
getGameGrid = _grid


getWindowSize :: Game -> (Int, Int)
getWindowSize = gameSize . view window 

initialGame :: IO Game
initialGame = do
  (s, g) <- getNextShape $ emptyGrid 10 20 
  return $ Game g initialWindow s 0
             

getNextShape :: GameGrid -> IO (Maybe TetrisShape , GameGrid)
getNextShape g = do
  gen <- newStdGen
  let s' = choose possibleShapes gen
  let s = map f s'
  let ng = addShapeToGrid s g
  return (Just $ MkShape s , ng)
                 where 
                   (w,h) = gridSize g
                   f (x, y) = ((w `div` 2) - x, h - 1 - y)
      

choose :: RandomGen b => [a] -> b -> a
choose xs g = let l = length xs in xs !! fst ( randomR (0, l -1) g)

possibleShapes :: [[(Int, Int)]]
possibleShapes = [[(0,0) , (0,1) , (1, 1), (1,0)],
                 [(1,0) , (0,0) ,  (2,0), (3,0)],
                 [(1,0) ,(0,0),  (2,0), (2,1)],
                 [(1,0) ,(0,0),  (2,0), (1,1)]]

initialWindow :: Window
initialWindow = MkWindow (300, 600)




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

removeShapeFromGrid :: [(Int,Int)] -> GameGrid -> GameGrid
removeShapeFromGrid = updateGridWithShape False

addShapeToGrid :: [(Int,Int)] -> GameGrid -> GameGrid
addShapeToGrid = updateGridWithShape True

updateGridWithShape :: Bool ->  [(Int,Int)] -> GameGrid -> GameGrid
updateGridWithShape b s g = foldr f g s
    where f (x, y) g' = setGridAt g' x y b
 
isValidPosition :: GameGrid -> [(Int, Int)] -> [(Int, Int)]->  Bool
isValidPosition g old new = not (outOfBounds || clashingBlock)
    where newPoints = filter (`notElem` old) new
          withinGrid (x , y) = let (x', y') = gridSize g in
                                 x < 0 || y < 0 || x >= x' || y >= y'
          clashingBlock = blockFilled g newPoints
          outOfBounds = L.any withinGrid newPoints

blockFilled :: GameGrid -> [(Int, Int)] -> Bool
blockFilled g =  L.any  (uncurry (valueInGridAt g))


moveShapePoints :: Direction -> [(Int, Int)] -> [(Int, Int)]
moveShapePoints (Translation d) points =  map  (directionToVector d) points
moveShapePoints Rotation points = let (axis, ps) = (,) <$> head <*> tail $ points in
                                  axis : map (rotate axis) ps
                                      
directionToVector :: (Num t1, Num t) => Translation -> (t, t1) -> (t, t1)
directionToVector d (x, y)= case d of 
                        DLeft -> (x-1, y)
                        DRight -> (x+1, y)
                        DDown -> (x, y-1)
                        DUp -> (x, y +1)

rotate :: (Int, Int) -> (Int, Int) -> (Int, Int)
rotate (ax, ay) (px,py) = (ax + dy , ay - dx)
   where  (dx, dy) = (px - ax, py - ay)
          

updateGravity :: Game -> IO Game
updateGravity g = if oldShape == newShape then shapePlaced newGame else return newGame
                  where oldShape = g ^. currentShape
                        newGame = moveShape (Translation DDown) g
                        newShape = newGame ^. currentShape

shapePlaced :: Game -> IO Game
shapePlaced = addNewShape . detectLoss . removeFullRows

addNewShape :: Game -> IO Game
addNewShape g = do
  (s, gr)  <- getNextShape $ g ^. grid
  return $ g  & currentShape .~ s
             & grid .~ gr
                 

detectLoss :: Game -> Game
detectLoss g = if L.or $ last $ g ^. grid  
               then error $ "Game over.  You scored: " ++ show ( g ^. score) 
               else g

removeFullRows :: Game -> Game
removeFullRows g = g & grid .~ newGrid
                    & score +~ (linesCleared * linesCleared)
    where gr = g ^. grid
          (w, h) = gridSize gr
          emptyRow = replicate w False
          (full, notFull) =  L.partition L.and  gr
          linesCleared = length full
          newGrid = take h ( notFull++ repeat emptyRow)
