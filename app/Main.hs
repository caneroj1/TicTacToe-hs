module Main where

import Lib
import Control.Monad
import Data.Text (unpack, split, pack)
import System.Console.Haskeline
import Data.List (maximumBy)
import Data.Ord (comparing)

outputBoard :: Board -> IO ()
outputBoard b = (putStrLn . unpack $ drawBoard b) >> putStrLn "\n"

getInput :: (Int -> Int -> Piece) -> IO Piece
getInput piece = do
  line <- runInputT defaultSettings $ getInputLine "Your move: "
  return $ processLn line
  where
    processLn Nothing   = undefined
    processLn (Just ln) = mkPiece
                        . map (read . unpack)
                        . split (== ',')
                        $ pack ln
    mkPiece [x, y] = piece x y

opponentMove :: Board -> Board
opponentMove board = addPiece (real maxMove) board
  where
    validMoves = moves board
    weighted   = zip (map (\p -> minimax (real p) board Maximize) validMoves) validMoves
    maxMove    = snd $ maximumBy (comparing fst) weighted
    real (Empty x y) = O x y

main :: IO ()
main = gameLoop emptyBoard P1
  where
    gameLoop gameBoard P1 = do
      outputBoard gameBoard
      if victory gameBoard allXs
        then putStrLn "Victory!"
        else do
          piece <- getInput X
          gameLoop (addPiece piece gameBoard) P2
    gameLoop gameBoard P2 = do
      let newBoard = opponentMove gameBoard
      if victory newBoard allOs
        then putStrLn "Defeat!"
        else
          gameLoop newBoard P1
