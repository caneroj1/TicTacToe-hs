module AI.Minimax
(
  minimax
, MinMax(..)
) where

import Game.Board
import Game.Piece
import Data.List (maximumBy)
import Data.Foldable (minimumBy)
import Data.Ord  (comparing)

data MinMax = Minimize |
              Maximize deriving Eq

minimax :: Board -> MinMax -> Bool -> (Int, Piece)
minimax board minmax isX
  | iWon      =
    case minmax of
      Maximize -> (1,  Empty 0 0)
      Minimize -> (-1, Empty 0 0)
  | iLost     =
    case minmax of
      Maximize -> (-1, Empty 0 0)
      Minimize -> (1,  Empty 0 0)
  | noMoves   =   (0,  Empty 0 0)
  | otherwise =
    case minmax of
      Maximize -> maximumBy (comparing fst) $
                  zip (map (fst . runNextMM board Minimize (not isX)) currMoves)
                      currMoves
      Minimize -> minimumBy (comparing fst) $
                  zip (map (fst . runNextMM board Maximize (not isX)) currMoves)
                      currMoves
  where
    currMoves     = map (emptyToMove isX) $ moves board
    iLost
      | not isX   = victory board allXs
      | otherwise = victory board allOs
    iWon
      | isX       = victory board allXs
      | otherwise = victory board allOs
    noMoves       = null currMoves
    runNextMM brd minOrMax isX piece =
      minimax (addPiece piece brd) minOrMax isX
