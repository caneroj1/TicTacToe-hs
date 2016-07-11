module AI.AlphaBeta
(
  alphaBeta
) where

import Game.Board
import Game.Piece
import Control.Monad.State
import AI.Minimax
import Data.Maybe (fromJust, isJust)
import Data.Foldable (minimumBy)
import AI.Heuristic
import Data.List (maximumBy)
import Data.Ord (comparing)

data AlphaBeta = Alpha Int | Beta Int   deriving Show
data Choice    = AlphaPrune | BetaPrune deriving Show

type Alpha = Int
type Beta  = Int
type Depth = Maybe Int

runMaxDepth :: Board -> MinMax -> Bool -> [Piece] -> (Int, Piece)
runMaxDepth _     _        _   []  = (0, Empty 0 0)
runMaxDepth board minmax isX moves =
  case minmax of
    Maximize -> heuristicBestMove maximumBy 1
    Minimize -> heuristicBestMove minimumBy (-1)
  where
    heuristicBestMove selectFn modifier =
      selectFn (comparing fst ) $
        zip (map ((* modifier) . flip numberOfConnectedTiles isX . flip addPiece board)
                  moves)
            moves

alphaBeta :: Board -> MinMax -> Bool -> Alpha -> Beta -> Depth -> (Int, Piece)
alphaBeta board minmax isX alpha beta depth
  | reachedMaxDepth = runMaxDepth board minmax isX currMoves

  | iWon      =
    case minmax of
      Maximize -> (100,  Empty 0 0)
      Minimize -> (-100, Empty 0 0)
  | iLost     =
    case minmax of
      Maximize -> (-100, Empty 0 0)
      Minimize -> (100,  Empty 0 0)

  | noMoves   =   (0,  Empty 0 0)

  | otherwise =
    case minmax of
      Maximize -> runAlphaBetaPrune currMoves board alpha beta BetaPrune
      Minimize -> runAlphaBetaPrune currMoves board alpha beta AlphaPrune
  where
    reachedMaxDepth = isJust depth && 0 == fromJust depth

    nd     =
      case depth of
        Just d -> Just (d - 1)
        _      -> Nothing

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
            v = fst $ alphaBeta (addPiece mv brd) Minimize (not isX) alpha bt nd
            newAlpha    = if v > alpha then Alpha v else Alpha alpha
            newBestMove = if v > alpha then Just mv else bestMove

        go (mv:mvss) board (Beta beta) bestMove
          | v < al    = (al, mv)
          | otherwise = go mvss brd newBeta newBestMove
          where
            v = fst $ alphaBeta (addPiece mv brd) Maximize (not isX) al beta nd
            newBeta     = if v < beta then Beta v else Beta beta
            newBestMove = if v < beta then Just mv else bestMove
