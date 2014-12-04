{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.Tetris(
drawTetrisBoard
) 
where

import Control.Lens ((^.))
import Data.Types
import Graphics.Grid
import Graphics.Gloss (Color, Picture,  rectangleSolid, color, blue, green, red, yellow)
import Graphics.Gloss (text)
import Data.Monoid ((<>))
import Graphics.Gloss (white)


drawTetrisBoard :: Game -> Picture
drawTetrisBoard board = (drawGrid drawTetrisBlock g (fromIntegral w) (fromIntegral h)) <>  (drawScore $ board ^. score)
    where
      (w, h) = gameSize $ board ^. window
      g = board ^. grid
         

drawScore :: Int -> Picture
drawScore i  = color white $ text (show i)

drawTetrisBlock :: TetrisColor -> Float -> Float -> Picture
drawTetrisBlock c w h  = color (glossColor c) (rectangleSolid w h) 

glossColor :: TetrisColor -> Color
glossColor c = case c of
               Blue -> blue
               Green -> green
               Red -> red
               Yellow -> yellow
               
