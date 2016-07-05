module AI.AlphaBeta
(
  alphaBeta
) where

import Game.Board
import Game.Piece
import Control.Monad.State
import AI.Minimax
import Data.Maybe (fromJust)

data AlphaBeta = Alpha Int | Beta Int   deriving Show
data Choice    = AlphaPrune | BetaPrune deriving Show

alphaBeta :: Board -> MinMax -> Bool -> Int -> Int -> (Int, Piece)
alphaBeta board minmax isX alpha beta
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
      Maximize -> runAlphaBetaPrune currMoves board alpha beta BetaPrune
      Minimize -> runAlphaBetaPrune currMoves board alpha beta AlphaPrune
  where
    currMoves     =  map (emptyToMove isX) $ moves board
    iLost
      | not isX   = victory board allXs
      | otherwise = victory board allOs
    iWon
      | isX       = victory board allXs
      | otherwise = victory board allOs
    noMoves       = null currMoves

    runAlphaBetaPrune mvs brd al bt choice =
      case choice of
        AlphaPrune -> go mvs brd (Beta bt ) Nothing -- when minimizing
        BetaPrune  -> go mvs brd (Alpha al) Nothing -- when maximizing
      where
        go []        _     (Alpha alpha) bestMove = (alpha, fromJust bestMove)
        go []        _     (Beta  beta ) bestMove = (beta,  fromJust bestMove)

        go (mv:mvss) board (Alpha alpha) bestMove
          | v > bt    = (bt, mv)
          | otherwise = go mvss brd newAlpha newBestMove
          where
            v = fst $ alphaBeta (addPiece mv brd) Minimize (not isX) alpha bt
            newAlpha    = if v > alpha then Alpha v else Alpha alpha
            newBestMove = if v > alpha then Just mv else bestMove

        go (mv:mvss) board (Beta beta) bestMove
          | v < al    = (al, mv)
          | otherwise = go mvss brd newBeta newBestMove
          where
            v = fst $ alphaBeta (addPiece mv brd) Maximize (not isX) al beta
            newBeta     = if v < beta then Beta v else Beta beta
            newBestMove = if v < beta then Just mv else bestMove
