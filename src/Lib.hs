module Lib
  (
    module Game.Board
  , module Game.Piece
  )
where

import Game.Board
import Game.Piece

someFunc :: IO ()
someFunc = putStrLn "someFunc"
