{-# LANGUAGE TemplateHaskell #-}
module Data.Types where
import Data.Grid
import Control.Lens (makeLenses)

data TetrisColor = Red | Green | Yellow | Blue deriving (Eq, Enum, Bounded)

data TetrisShape = MkShape {_blocks :: [(Int,Int)], _tetrisColor :: TetrisColor} deriving Eq

type TetrisBlock = TetrisColor


type GameGrid = Grid TetrisBlock

newtype Window = MkWindow {gameSize :: (Int, Int)}

data Game = Game { _grid :: GameGrid,  
                   _window :: Window, 
                   _currentShape :: Maybe TetrisShape , 
                   _score :: Int}

makeLenses ''TetrisShape
makeLenses ''Game
