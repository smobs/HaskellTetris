{-# LANGUAGE TemplateHaskell #-}

module Data.Grid(
getRows,
Grid,
fmap,
emptyGrid,
gridSize,
lastRow,
rowsToGrid,
setGridAt,
valueInGridAt
) where

import Control.Lens ((&), (^.), makeLenses, (%~))
import qualified Data.Map as M
import Data.Map.Strict (Map)

data Grid a = MkGrid {_gridMap :: Map (Int, Int) a,
                      _gridLimits :: (Int, Int) }
            deriving (Show, Eq)

makeLenses ''Grid

instance Functor Grid where 
    fmap f = gridMap %~ (M.map f) 

emptyGrid :: Int -> Int -> Grid a
emptyGrid x y = MkGrid M.empty (x,y)

gridSize :: Grid a -> (Int, Int)
gridSize g = g ^. gridLimits 

--yuck
setGridAt :: Int -> Int -> Maybe a -> Grid a -> Grid a
setGridAt x y v g = if inBound 
                    then g & (gridMap  %~ updateAction) 
                    else g
    where
      (w, h) = gridSize g
      inBound = and [x >= 0, y >= 0, x < w, y < h] 
      updateAction = case v of 
                       Nothing -> M.delete (x , y)
                       Just v' -> M.insert (x, y) v'
                           
valueInGridAt ::  Int -> Int -> Grid a -> Maybe a
valueInGridAt x y g = M.lookup (x, y) $ g ^. gridMap

rowsToGrid :: [[Maybe a]] -> Grid a
rowsToGrid [] = emptyGrid 0 0
rowsToGrid ss = foldr (update) (emptyGrid x y) (concat $ labelElements ss)
    where 
      y = length ss
      x = length $ head ss
      update ((px,py), v) = setGridAt px py v 

labelElements :: [[a]] -> [[((Int, Int), a)]]
labelElements ss = zipWith f  [0 .. ] ss
                   where f i rs  = zip (map (\j -> (j, i)) [0..]) rs

lastRow :: Grid a -> [Maybe a]
lastRow g = getRow ((snd $  gridSize g) -1) g

getRows :: Grid a -> [[Maybe a]]
getRows g = [getRow y g |
            y <- [0.. (snd $  gridSize g)-1]
            ]

getRow :: Int -> Grid a -> [Maybe a]
getRow i g = 
    [ valueInGridAt x i g | 
      let maxX = fst $ gridSize g,
      x <- [0.. maxX - 1]] 
