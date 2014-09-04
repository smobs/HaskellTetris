{-# LANGUAGE TupleSections #-}
import Control.Applicative ((<$>))
import Control.Concurrent (MVar, forkIO, putMVar, threadDelay)
import Control.Lens ((.~), ix)
import Control.Monad (void)
import Data.Monoid ((<>), mconcat)

import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)
import Control.Concurrent (newEmptyMVar)
import Control.Concurrent (tryTakeMVar)



data Play = X | O deriving Eq

type Board = [[Maybe Play]]

initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Nothing)

main :: IO()
main = do
  aiMove <- newEmptyMVar
  playIO
       (InWindow "Tic-tac-toe" (500, 500) (500, 500))
       azure
       10
       (initialBoard, X)
       drawBoard
       (handleInput aiMove)
       (stepGame aiMove)

stepGame :: MVar Board -> Float -> (Board, Play) -> IO (Board, Play)
stepGame aiMove _ (board, O) =
    tryTakeMVar aiMove >>= 
                return . maybe (board, O) ((, X))
stepGame _ _ state = return state

handleInput :: MVar Board -> Event -> (Board, Play) -> IO (Board, Play)
handleInput
  aiMove
  (EventKey (MouseButton LeftButton) Up _ (x,y))
  (board, X) =
    let snap = (+1) . min 1 . max (-1) . fromIntegral .floor. (/ 100). (+50)
        gridX = snap x
        gridY = snap y
    in case (board !! gridX) !! gridY of
      Just _ -> return (board, X)
      Nothing -> do
                 let newBoard = (ix gridX . ix gridY .~ Just X) board
                 forkAi aiMove newBoard
                 return (newBoard, O)

handleInput _ _ b  = return b
    


drawBoard :: (Board, Play) -> IO Picture
drawBoard (board, _) = return (grid <> plays) 
    where
      grid = color black (line ([(-100, -300), (-100, 300)])) <>
             color black (line ([(100, -300), (100, 300)])) <>
             color black (line ([(-300, 100), (300, 100)])) <>
             color black (line ([(-300, -100), (300, -100)]))
      plays = mconcat
              [ translate (fromIntegral $ (x - 1) * 200)
                        (fromIntegral $ (y - 1) * 200) $
                case play of
                  X -> color white (thickCircle 1 50)
                  O -> color black (thickCircle 1 50)
              | x <- [0..2]
              , y <- [0..2]
              , Just play <- [(board !! x) !! y]
              ]

forkAi :: MVar Board -> Board -> IO ()
forkAi aiMove board = void $ forkIO $ do
                        randomRIO (100000, 1000000) >>= threadDelay
                        
                        let plays = [(ix x . ix y .~ Just O) board
                                     | x <- [0..2]
                                     , y <- [0..2]
                                     , Nothing <- [(board !! x) !! y]
                                     ]
                        case plays of
                          [] -> putMVar aiMove board
                          _ -> do
                            newBoard <- (plays !!) <$> randomRIO (0, length plays - 1)
                            putMVar aiMove newBoard
                        
