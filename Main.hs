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
       2
       game
       drawBoard
       handleInput
       stepGame

stepGame :: Float -> Game -> IO Game
stepGame _ =  updateGravity 

handleInput ::  Event -> Game -> IO Game
handleInput (EventKey (SpecialKey KeyLeft) Down  _ _) = return . move (Translation DLeft)
handleInput (EventKey (SpecialKey KeyRight) Down  _ _) = return . move (Translation DRight)
handleInput (EventKey (SpecialKey KeyUp) Down  _ _) = return . move Rotation
handleInput (EventKey (SpecialKey KeyDown) Down  _ _) = return . move (Translation DDown)
handleInput  _  = return
    

drawBoard :: Game -> IO Picture
drawBoard = return .  drawTetrisBoard
                       



