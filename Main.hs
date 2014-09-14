{-# LANGUAGE TupleSections #-}

import Graphics.Gloss.Interface.IO.Game
import Data.Tetris
import Graphics.Tetris

main :: IO()
main = do
  game <- initialGame
  playIO
       (InWindow "Tetris" (getWindowSize game) (500, 500))
       black
       1
       game
       drawBoard
       handleInput
       stepGame

stepGame :: Float -> Game -> IO Game
stepGame _ =  updateGravity 

handleInput ::  Event -> Game -> IO Game
handleInput (EventKey (SpecialKey KeyLeft) Down  _ _) = return . moveShape DLeft
handleInput (EventKey (SpecialKey KeyRight) Down  _ _) = return . moveShape DRight
handleInput (EventKey (SpecialKey KeyUp) Down  _ _) = return . moveShape DUp
handleInput (EventKey (SpecialKey KeyDown) Down  _ _) = return . moveShape DDown
handleInput  _  = return
    

drawBoard :: Game -> IO Picture
drawBoard = return .  drawTetrisBoard
                       



