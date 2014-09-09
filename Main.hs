{-# LANGUAGE TupleSections #-}
import Data.Monoid ((<>), mconcat)

import Graphics.Gloss.Interface.IO.Game
import Data.Monoid (mappend)

data Game = Game { grid :: Grid, window :: Window }

type Grid = [[Bool]]
data Window = Window {game_size :: (Int, Int)}

initialGame :: Game
initialGame = Game initialGrid initialWindow

initialWindow :: Window
initialWindow = Window (500, 500)

initialGrid :: Grid
initialGrid = replicate 3 [False, False, True]

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
drawBoard board = return (gridLines <> filledSquares) 
    where
      drawLine = color red . line
      gridLines = (mconcat . map  drawLine) 
                  [[(-100, -300), (-100, 300)] ,
                   [(100, -300), (100, 300)] ,
                   [(-300, 100), (300, 100)] ,
                   [(-300, -100), (300, -100)]]
      filledSquares = mconcat
              [ translate (fromIntegral $ (x - 1) * 200)
                        (fromIntegral $ (y - 1) * 200) $
                if filled then
                  color white (thickCircle 1 50)
                  else  color black (thickCircle 1 50)
              | x <- [0..2]
              , y <- [0..2]
              , filled <- [(grid board !! x) !! y]
              ]
                        
