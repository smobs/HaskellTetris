{-# LANGUAGE TupleSections #-}
import Data.Monoid ((<>), mconcat)

import Graphics.Gloss.Interface.IO.Game
import Data.Tuple (swap)
import Control.Applicative ((<$>))
import Control.Applicative ((<*>))

data Game = Game { game_grid :: Grid, window :: Window }

type Grid = [[Bool]]
data Window = Window {game_size :: (Int, Int)}

initialGame :: Game
initialGame = Game initialGrid initialWindow

initialWindow :: Window
initialWindow = Window (500, 500)

initialGrid :: Grid
initialGrid = replicate 5 [False, True, False, True]

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
drawBoard board = return (drawSquares s grid <> uncurry gridLines (gridSize grid) s) 
    where
      s = (fst . game_size . window) board
      grid = game_grid board
                       
filledSquares :: Float -> Grid -> Picture
filledSquares s grid = mconcat
              [ drawSquareAt (translateCornerTo nx x) (translateCornerTo ny y) (sqWidth nx) (sqWidth ny)
              | x <- [0 .. nx - 1]
              , y <- [0.. ny - 1]
              , True <- [(grid !! y) !! x]
              ]
    where (nx , ny) = gridSize grid
          sqWidth n = s / fromIntegral  n
          translateCornerTo n i = translateCoord s n ((fromIntegral i) + 0.5)
         


          
drawSquareAt :: Float -> Float -> Float -> Float  -> Picture
drawSquareAt x y w h  =  translate x y $
                        color white (rectangleSolid w h)
    

gridSize :: Grid -> (Int , Int)
gridSize = (,) <$> length . head <*> length

drawSquares :: Int -> Grid -> Picture
drawSquares s grid = filledSquares (fromIntegral s) grid

gridLines :: Int -> Int -> Int -> Picture
gridLines nw nh  = drawLines . getLinePoints nw nh

drawLines :: [Path] -> Picture
drawLines =  mconcat . map  drawLine
    where drawLine = color red . line

getLinePoints :: Int -> Int -> Int -> [Path]
getLinePoints nw nh size = getGridPaths nw nh  s s
                     where s = fromIntegral size

intervals :: Int -> Float -> [Float]
intervals n s = map ((translateCoord s n) . fromIntegral) [0 .. n]

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
      

