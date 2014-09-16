module Data.Grid(
Grid,
setGridAt,
emptyGrid,
valueInGridAt,
gridSize
) where

import Control.Applicative ((<*>))
import Control.Applicative ((<$>))

type Grid a = [[a]]


gridSize :: Grid a -> (Int , Int)
gridSize = (,) <$> length . head <*> length 

valueInGridAt :: Grid a -> Int -> Int -> a
valueInGridAt g x y =  g !! y !! x

--yuck
setGridAt :: Grid a -> Int -> Int -> a -> Grid a
setGridAt g x y v = update y row g
                    where row = update x (const v)

update ::Int -> (a -> a) -> [a] -> [a] 
update i f xs = zipWith repl xs [0..]
    where repl a i'  | i == i' = f a
                        | otherwise = a

emptyGrid :: Int -> Int -> Grid Bool
emptyGrid x y = replicate y $ replicate x False 
