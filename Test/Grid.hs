{-# LANGUAGE ViewPatterns #-}
module Test.Grid where

import Test.QuickCheck
import Data.Grid

type TestT = Int

test :: IO ()
test = do
    quickCheck gridRowSizeIsPositive
    quickCheck gridColSizeIsPositive
    quickCheck emptyGridIsTheRightSize
    quickCheck emptyGridIsEmpty

gridRowSizeIsPositive :: Grid TestT -> Bool
gridRowSizeIsPositive  = \gr -> fst (gridSize gr) >= 0

gridColSizeIsPositive ::  Grid TestT -> Bool
gridColSizeIsPositive  = \gr -> snd (gridSize gr) >= 0


emptyGridIsTheRightSize :: (Positive Int, Positive Int) -> Bool
emptyGridIsTheRightSize (getPositive -> x, getPositive -> y) =
    gridSize(emptyGrid x y) == (x, y)

emptyGridIsEmpty :: Grid TestT -> NonNegative Int -> NonNegative Int -> Maybe TestT -> Property
emptyGridIsEmpty g (getNonNegative -> x) (getNonNegative -> y) v = 
    let (w, h) = gridSize g in
    x < w && y < h ==> 
      let g' = setGridAt g x y v in
      collect g' $ valueInGridAt g' x y == v
