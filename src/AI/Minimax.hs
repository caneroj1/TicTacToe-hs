module AI.Minimax
(
  minimax
, MinMax(..)
) where

import Game.Board
import Game.Piece

data MinMax = Minimize |
              Maximize deriving Eq

minimax :: Board -> MinMax -> Bool -> Piece -> Int
minimax board minmax isX piece
  | iWon      =
    case minmax of
      Maximize -> 1
      Minimize -> -1
  | iLost     =
    case minmax of
      Maximize -> -1
      Minimize -> 1
  | noMoves   = 0
  | otherwise =
    case minmax of
      Maximize -> minimum $ map (minimax newBoard Minimize (not isX) ) myMoves
      Minimize -> maximum $ map (minimax newBoard Maximize (not isX) ) myMoves
  where
    newBoard      = addPiece piece board
    iLost
      | not isX   = victory newBoard allXs
      | otherwise = victory newBoard allOs
    iWon
      | isX       = victory newBoard allXs
      | otherwise = victory newBoard allOs
    myMoves       = map (emptyToMove (not isX)) $ moves newBoard
    noMoves       = null myMoves
