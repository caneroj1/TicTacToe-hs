module Main where

import Lib
import Control.Monad
import Data.Text (unpack, split, pack)
import System.Console.Haskeline

data Player = P1 | P2 deriving Eq

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

main :: IO ()
main = gameLoop emptyBoard P1
  where
    gameLoop gameBoard P1 = do
      outputBoard gameBoard
      piece <- getInput X
      gameLoop (addPiece piece gameBoard) P2
    gameLoop gameBoard P2 = do
      outputBoard gameBoard
      piece <- getInput O
      gameLoop (addPiece piece gameBoard) P1
