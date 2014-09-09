{-# LANGUAGE TupleSections #-}
import Data.Monoid ((<>), mconcat)

import Graphics.Gloss.Interface.IO.Game

data Game = Game { grid :: Grid, window :: Window }

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
drawBoard board = return (filledSquares <> gridLines) 
    where
      size = (fst . game_size . window) board
      s = squareWidth size
      drawLine = color red . line
      gridLines = (mconcat . map  drawLine . getLinePoints) size                 
      filledSquares = mconcat
              [ translate (fromIntegral  (x - 1) * s)
                        (fromIntegral  (y - 1) * s) $
                
                  color white (rectangleSolid s s)
                 
              | x <- [0..2]
              , y <- [0..2]
              , True <- [(grid board !! x) !! y]
              ]
                        

getLinePoints :: Int -> [Path]
getLinePoints size = [[(-l, -h), (-l, h)] ,
                   [(l, -h), (l, h)] ,
                   [(-h, l), (h, l)] ,
                   [(-h, -l), (h, -l)]]
    where 
      h = fromIntegral (size `div` 2)
      l =  fromIntegral (size `div`  6)
           
squareWidth :: Int -> Float
squareWidth size = (fromIntegral . div size) 3
      
