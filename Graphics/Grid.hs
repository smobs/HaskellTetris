{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Graphics.Grid 
(drawGrid)
where

import Data.Grid
import Data.Monoid ((<>), mconcat)
import Data.Tuple (swap)
import Graphics.Gloss ( color, translate, Path, Picture, black, line)

type DrawableWithSize = Float -> Float -> Picture

drawGrid :: (a ->  DrawableWithSize) -> Grid a -> DrawableWithSize
drawGrid d g w h = drawSquares g' w h <> uncurry gridLines (gridSize g) w h
                   where g' = fmap d g 


drawSquares :: Grid DrawableWithSize -> DrawableWithSize
drawSquares g sw sh = mconcat
                        [ drawSquareAt 
                          (d (sqWidthX nx)  (sqWidthY ny))
                          (translateCornerToX nx x) 
                          (translateCornerToY ny y) 
                          | x <- [0 .. nx - 1]
                        , y <- [0.. ny - 1]
                        , Just d <- [valueInGridAt x y g]
              ]
    where (nx , ny) = gridSize g
          sqWidthX n = sw / fromIntegral n
          translateCornerToX n i = translateCoord sw n (fromIntegral i + 0.5)
          sqWidthY n = sh / fromIntegral n
          translateCornerToY n i = translateCoord sh n (fromIntegral i + 0.5)

            
drawSquareAt :: Picture -> DrawableWithSize
drawSquareAt p x y  =  translate x y p


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

gridLines :: Int -> Int -> DrawableWithSize
gridLines nw nh sw = drawLines . getGridPaths nw nh sw

drawLines :: [Path] -> Picture
drawLines =  mconcat . map  drawLine
    where drawLine = color black . line


intervals :: Int -> Float -> [Float]
intervals n s = map (translateCoord s n . fromIntegral) [0 .. n]
