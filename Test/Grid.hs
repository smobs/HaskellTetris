{-# LANGUAGE ViewPatterns #-}
module Test.Grid where

import Test.QuickCheck
import Data.Grid
import Control.Lens ((^.))

type TestT = Int

instance Arbitrary a => Arbitrary (Grid a) where
    arbitrary = do
      NonNegative x <- arbitrary
      NonNegative y <- arbitrary
      rs <- fixedLengthListOfList x y
      return $ rowsToGrid rs


fixedLengthListOfList :: Arbitrary a => Int -> Int -> Gen [[a]]
fixedLengthListOfList x y = do
  sequence [ fixedLengthArbList x | i <- [0..y]]


fixedLengthArbList :: Arbitrary a => Int -> Gen [a]
fixedLengthArbList n = do
  sequence [ arbitrary | i <- [0..n]]

test :: IO ()
test = do
  quickCheck toRowsAndFromRowsAreId
  quickCheck gridRowSizeIsPositive
  quickCheck gridColSizeIsPositive
  quickCheck emptyGridIsTheRightSize
  --quickCheck emptyGridIsEmpty

toRowsAndFromRowsAreId :: Grid TestT -> Property
toRowsAndFromRowsAreId ts = let t' = (rowsToGrid $ getRows ts) in 
                            counterexample (show t') $ t' == ts 

gridRowSizeIsPositive :: Grid TestT -> Bool
gridRowSizeIsPositive  = \gr -> fst ( gridSize gr) >= 0

gridColSizeIsPositive ::  Grid TestT -> Bool
gridColSizeIsPositive  = \gr -> snd (gridSize gr) >= 0


emptyGridIsTheRightSize :: (Positive Int, Positive Int) -> Bool
emptyGridIsTheRightSize (getPositive -> x, getPositive -> y) =
     gridSize (emptyGrid x y) == (x, y)

emptyGridIsEmpty :: Grid TestT -> NonNegative Int -> NonNegative Int -> Maybe TestT -> Property
emptyGridIsEmpty g (getNonNegative -> x) (getNonNegative -> y) v = 
    let (w, h) = gridSize g in
    x < w && y < h ==> 
      let g' = setGridAt x y v g in
      counterexample (show g') $ valueInGridAt x y g' == v
