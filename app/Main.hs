module Main where

import Lib
import Control.Monad
import Data.Text (unpack)

outputBoard :: Board -> IO ()
outputBoard b = (putStrLn . unpack $ drawBoard b) >> putStrLn "\n-----\n"

main :: IO ()
main = do
  outputBoard emptyBoard
  let b1 = addPiece (X 1 0) emptyBoard
  outputBoard b1
  let b2 = addPiece (O 1 1) b1
  outputBoard b2
  let b3 = addPiece (X 2 2) b2
  outputBoard b3
