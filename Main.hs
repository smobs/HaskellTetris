{-# LANGUAGE TupleSections #-}
import Data.Monoid ((<>), mconcat)

import Graphics.Gloss.Interface.IO.Game
import Data.Tuple (swap)
import Control.Applicative ((<$>))
import Control.Applicative ((<*>))

data Game = Game { game_grid :: Grid, window :: Window }

type Grid = [[Bool]]
data Window = Window {game_size :: (Int, Int)}

initialGame :: Game
initialGame = Game initialGrid initialWindow

initialWindow :: Window
initialWindow = Window (500, 500)

initialGrid :: Grid
initialGrid = replicate 3 [True, False, True]

main :: IO()
main = do
  let game = initialGame
  playIO
       (InWindow "Tetris" (game_size $ window game) (500, 500))
       black
       10
       game
       drawBoard
       handleInput
       stepGame

stepGame :: Float -> Game -> IO Game
stepGame _ = return

handleInput ::  Event -> Game -> IO Game
handleInput  _  = return
    

drawBoard :: Game -> IO Picture
drawBoard board = return (filledSquares (squareWidth s) (game_grid board) <> gridLines s) 
    where
      s = (fst . game_size . window) board
      
                       
      

filledSquares :: Float -> Grid -> Picture
filledSquares s grid = mconcat
              [ translate (fromIntegral (x - 1) * s)
                        (fromIntegral (y - 1) * s) $
                        color white (rectangleSolid s s)
              | x <- [0..2]
              , y <- [0..2]
              , True <- [(grid !! x) !! y]
              ]
                        

gridLines :: Int -> Picture
gridLines  = mconcat . map  drawLine . getLinePoints
    where drawLine = color red . line

getLinePoints :: Int -> [Path]
getLinePoints size = getGridPaths 6  s s
                     where s = fromIntegral size

intervals :: Int -> Float -> [Float]
intervals n s = map ((*) $ s / n') [-n'' .. n'']  
    where  n' = (fromIntegral n) 
           n'' = n' / 2

getGridPaths :: Int -> Float -> Float -> [Path]
getGridPaths n w h = (++) vertCoords horCoords
                     where vertCoords = map (verticalLine h) (intervals n w)
                           horCoords = map (horizontalLine w) (intervals n h)
                           


verticalLine :: Float -> Float -> Path
verticalLine l pos  = [(pos, -l'), (pos, l')]
                     where l' =  l / 2

horizontalLine :: Float -> Float -> Path
horizontalLine l  = map swap .  verticalLine l


squareWidth :: Int -> Float
squareWidth size = (fromIntegral . div size) 3
      

