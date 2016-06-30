module AI.Minimax
(
  minimax
, MinMax(..)
) where

import Game.Board
import Game.Piece

data MinMax = Minimize |
              Maximize deriving Eq

minimax :: Board -> MinMax -> Piece -> Int
minimax board Maximize piece
  | victoryForMe    = 1
  | victoryForThem  = -1
  | null validMoves = 0
  | otherwise       = minimum $ map (minimax newBoard Minimize) validMoves
  where
    newBoard       = addPiece piece board
    victoryForMe   = victory newBoard allOs
    victoryForThem = victory newBoard allXs
    validMoves     = map emptyToMove $ moves newBoard
    emptyToMove (Empty x y) = X x y
minimax board Minimize piece
  | victoryForMe    = -1
  | victoryForThem  = 1
  | null validMoves = 0
  | otherwise       = maximum $ map (minimax newBoard Maximize) validMoves
  where
    newBoard       = addPiece piece board
    victoryForMe   = victory newBoard allXs
    victoryForThem = victory newBoard allOs
    validMoves     = map emptyToMove $ moves newBoard
    emptyToMove (Empty x y) = O x y
