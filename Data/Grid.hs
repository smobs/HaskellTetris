module Data.Grid(
Grid,
emptyGrid,
gridMap,
gridSize,
setGridAt,
valueInGridAt
) where

import Control.Applicative ((<$>), (<*>))

type Grid a = [[Maybe a]]

emptyGrid :: Int -> Int -> Grid a
emptyGrid x y  = replicate y $ replicate x Nothing

gridMap :: (a -> b) -> Grid a -> Grid b
gridMap f = map $ map $ fmap f

gridSize :: Grid a -> (Int , Int)
gridSize [] = (0, 0)
gridSize g = (,) <$> length . head <*> length $ g 

--yuck
setGridAt :: Grid a -> Int -> Int -> Maybe a -> Grid a
setGridAt g x y v = update y row g
                    where row = update x (const v)

valueInGridAt :: Grid a -> Int -> Int -> Maybe a
valueInGridAt g x y =  g !! y !! x

update ::Int -> (a -> a) -> [a] -> [a] 
update i f xs = zipWith repl xs [0..]
    where repl a i'  | i == i' = f a
                        | otherwise = a


