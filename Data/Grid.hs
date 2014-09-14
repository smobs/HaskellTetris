module Data.Grid(
Grid,
setGridAt,
initialGrid,
valueInGridAt,
gridSize
) where

import Control.Applicative ((<*>))
import Control.Applicative ((<$>))

type Grid = [[Bool]]


gridSize :: Grid -> (Int , Int)
gridSize = (,) <$> length . head <*> length 

initialGrid :: Grid
initialGrid =  replicate 10 $ replicate 10 False

valueInGridAt :: Grid -> Int -> Int -> Bool
valueInGridAt g x y =  g !! y !! x

--yuck
setGridAt :: Grid -> Int -> Int -> Bool -> Grid
setGridAt g x y v = update y row g
                    where row = update x (const v)

update :: Eq a => Int -> (a -> a) -> [a] -> [a] 
update i f xs = zipWith repl xs [0..]
    where repl a i'  | i == i' = f a
                        | otherwise = a

