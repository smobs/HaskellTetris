{-# LANGUAGE TupleSections #-}

import Graphics.Gloss.Interface.IO.Game
import Types.Tetris
import Graphics.Tetris

main :: IO()
main = do
  let game = initialGame
  playIO
       (InWindow "Tetris" (getWindowSize game) (500, 500))
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
drawBoard = return .  drawTetrisBoard
                       



