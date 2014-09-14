module Graphics.Tetris(
drawTetrisBoard
) where

import Data.Monoid ((<>))
import Data.Monoid (mconcat)
import Data.Tuple (swap)
import Graphics.Gloss (Path, Picture, color, line, rectangleSolid, red, translate, white)
import Data.Tetris

drawTetrisBoard :: Game -> Picture
drawTetrisBoard board =  (drawSquares w h grid <> uncurry gridLines (gridSize grid) w h) 
    where
      (w, h) = getWindowSize board
      grid = getGameGrid board
         

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
      

filledSquares :: Float -> Float -> Grid Bool -> Picture
filledSquares sw sh grid = mconcat
              [ drawSquareAt (translateCornerToX nx x) (translateCornerToY ny y) (sqWidthX nx) (sqWidthY ny)
              | x <- [0 .. nx - 1]
              , y <- [0.. ny - 1]
              , True <- [valueInGridAt grid x y]
              ]
    where (nx , ny) = gridSize grid
          sqWidthX n = sw / fromIntegral n
          translateCornerToX n i = translateCoord sw n ((fromIntegral i) + 0.5)
          sqWidthY n = sh / fromIntegral n
          translateCornerToY n i = translateCoord sh n ((fromIntegral i) + 0.5)
         


          
drawSquareAt :: Float -> Float -> Float -> Float  -> Picture
drawSquareAt x y w h  =  translate x y $
                        color white (rectangleSolid w h)
    


drawSquares :: Int -> Int -> Grid Bool -> Picture
drawSquares sw sh  = filledSquares (fromIntegral sw) (fromIntegral sh) 

gridLines :: Int -> Int -> Int -> Int -> Picture
gridLines nw nh sw = drawLines . getLinePoints nw nh sw

drawLines :: [Path] -> Picture
drawLines =  mconcat . map  drawLine
    where drawLine = color red . line

getLinePoints :: Int -> Int -> Int -> Int -> [Path]
getLinePoints nw nh sw sh = getGridPaths nw nh  sw' sh'
                     where sw' = fromIntegral sw
                           sh' = fromIntegral sh

intervals :: Int -> Float -> [Float]
intervals n s = map ((translateCoord s n) . fromIntegral) [0 .. n]

translateCoord :: Float -> Int -> Float -> Float
