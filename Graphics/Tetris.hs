{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.Tetris(
drawTetrisBoard
) where


import Control.Lens ((^.))
import Data.Types
import Graphics.Types
import Graphics.Grid ()
import Graphics.Gloss (Color, Picture,  rectangleSolid, color, blue, green, red, yellow)



instance Drawable Game where
    draw g x y = drawTetrisBoard g

instance Drawable TetrisBlock where 
    draw = drawTetrisBlock

drawTetrisBoard :: Game -> Picture
drawTetrisBoard board = draw g (fromIntegral w) (fromIntegral h)
    where
      (w, h) = gameSize $ board ^. window
      g = board ^. grid
         

drawTetrisBlock :: TetrisColor -> Float -> Float -> Picture
drawTetrisBlock c w h  = color (glossColor c) (rectangleSolid w h) 

glossColor :: TetrisColor -> Color
glossColor c = case c of
               Blue -> blue
               Green -> green
               Red -> red
               Yellow -> yellow
               
