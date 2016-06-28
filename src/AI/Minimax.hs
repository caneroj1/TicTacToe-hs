module AI.Minimax
(
  minimax
, MinMax(..)
) where

import Game.Board
import Game.Piece

data MinMax = Minimize |
              Maximize deriving Eq

minimax :: Piece -> Board -> MinMax -> Int
minimax piece board Maximize =
  if victoryForMe
    then 1
    else
      if victoryForThem
        then (-1)
        else
          if null validMoves
            then 0
            else
              minimum (
                map (\p -> minimax p newBoard Minimize) validMoves)
  where
    newBoard       = addPiece piece board
    victoryForMe   = victory newBoard allOs
    victoryForThem = victory newBoard allXs
    validMoves     = map emptyToMove $ moves newBoard
    emptyToMove (Empty x y) = X x y
minimax piece board Minimize =
  if victoryForMe
    then (-1)
    else
      if victoryForThem
        then 1
        else
          if null validMoves
            then 0
            else
              maximum (
                map (\p -> minimax p newBoard Maximize) validMoves)
  where
    newBoard       = addPiece piece board
    victoryForMe   = victory newBoard allXs
    victoryForThem = victory newBoard allOs
    validMoves     = map emptyToMove $ moves newBoard
    emptyToMove (Empty x y) = O x y
