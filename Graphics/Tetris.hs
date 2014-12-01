module Graphics.Tetris(
drawTetrisBoard
) where

import Data.Monoid ((<>), mconcat)
import Data.Types
import Data.Tuple (swap)
import Data.Grid
import Graphics.Gloss (color, line, red, translate,blue, green, yellow)

import Control.Lens ((^.))
import Graphics.Gloss (Picture)
import Graphics.Gloss (Path)
import Graphics.Gloss (Color)
import Graphics.Gloss (rectangleSolid)
import Graphics.Gloss (black)

drawTetrisBoard :: Game -> Picture
drawTetrisBoard board =  drawSquares w h g <> uncurry gridLines (gridSize g) w h
    where
      (w, h) = gameSize $ board ^. window
      g = board ^. grid
         
translateCoord :: Float -> Int -> Float -> Float
translateCoord s n i = s * (i / n')  - (s / 2)
    where  n' = fromIntegral n 

getGridPaths :: Int -> Int -> Float -> Float -> [Path]
getGridPaths nw nh  w h = (++) vertCoords horCoords
                     where
                       f g n s = map g $ intervals n s
                       vertCoords = f (verticalLine h) nw w
                       horCoords = f (horizontalLine w) nh h
                           


verticalLine :: Float -> Float -> Path
verticalLine l pos  = [(pos, -l'), (pos, l')]
                     where l' =  l / 2

horizontalLine :: Float -> Float -> Path
horizontalLine l  = map swap .  verticalLine l
      

filledSquares :: Float -> Float -> Grid TetrisColor -> Picture
filledSquares sw sh g = mconcat
              [ drawSquareAt 
                (glossColor c) 
                (translateCornerToX nx x) 
                (translateCornerToY ny y) 
                (sqWidthX nx) 
                (sqWidthY ny)
              | x <- [0 .. nx - 1]
              , y <- [0.. ny - 1]
              , Just c <- [valueInGridAt g x y]
              ]
    where (nx , ny) = gridSize g
          sqWidthX n = sw / fromIntegral n
          translateCornerToX n i = translateCoord sw n ((fromIntegral i) + 0.5)
          sqWidthY n = sh / fromIntegral n
          translateCornerToY n i = translateCoord sh n ((fromIntegral i) + 0.5)
             
drawSquareAt :: Color -> Float -> Float -> Float -> Float  -> Picture
drawSquareAt c x y w h  =  translate x y $
                        color c (rectangleSolid w h)
    
glossColor :: TetrisColor -> Color
glossColor c = case c of
               Blue -> blue
               Green -> green
               Red -> red
               Yellow -> yellow
               


drawSquares :: Int -> Int -> Grid TetrisColor -> Picture
drawSquares sw sh  = filledSquares (fromIntegral sw) (fromIntegral sh) 

gridLines :: Int -> Int -> Int -> Int -> Picture
gridLines nw nh sw = drawLines . getLinePoints nw nh sw

drawLines :: [Path] -> Picture
drawLines =  mconcat . map  drawLine
    where drawLine = color black . line

getLinePoints :: Int -> Int -> Int -> Int -> [Path]
getLinePoints nw nh sw sh = getGridPaths nw nh  sw' sh'
                     where sw' = fromIntegral sw
                           sh' = fromIntegral sh

intervals :: Int -> Float -> [Float]
intervals n s = map ((translateCoord s n) . fromIntegral) [0 .. n]

