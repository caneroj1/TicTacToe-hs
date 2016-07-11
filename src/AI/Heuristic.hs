module AI.Heuristic
(
  sumOfWinPaths
, numberOfConnectedTiles
) where

import Game.Piece
import Game.Board
import Data.List (partition, find)
import qualified Data.Set as S
import Control.Arrow ((&&&))
import Data.Maybe (isJust, isNothing)

scoreForlist :: Bool -> [Piece] -> Int
scoreForlist isX
  | isX       = diff . partition allXs
  | otherwise = diff . partition allOs
  where
    diff (trueGroup, falseGroup) = length trueGroup - length falseGroup

sumOfWinPaths :: Board -> Bool -> Int
sumOfWinPaths board isX =
  getScoreFor Row maxRowIndex    +
  getScoreFor Col maxColumnIndex +
  getDiagScore L                 +
  getDiagScore R
  where
    getScoreFor loc maxIdx =
      sum $ map (scoreForlist isX . piecesAtLoc board . loc) [0..maxIdx]
    getDiagScore           =
      scoreForlist isX . piecesAtLoc board . Diag

numberOfConnectedTiles :: Board -> Bool -> Int
numberOfConnectedTiles board isX =
  go myPieces S.empty
  where
    myPieces
      | isX       = pieceCoords allXs
      | otherwise = pieceCoords allOs

    pieceCoords fn = map (getX &&& getY) $ getPieces board fn

    go []     _        = 0
    go (x:xs) set
      | S.member x set = go xs set
      | otherwise      =
        let nextSet       = S.insert x set
            (val, newSet) = checkNeighbors xs (nextPieces xp yp) nextSet
          in val + go xs newSet
      where
        xp = fst x
        yp = snd x

        nextPieces xp yp =
            [(xp + x, yp + y) | x <- [-1, 0, 1],
                                y <- [-1, 0, 1],
                                (xp + x, yp + y) /= (xp, yp)]

        checkNeighbors currPieces [] set = (0, set)
        checkNeighbors currPieces adjacentList@(a:as) set
          | S.member a set                    = checkNeighbors currPieces as set
          | isNothing $ find (==a) currPieces =
            checkNeighbors currPieces as nextSet
          | otherwise                         =
            let (val, newSet) = checkNeighbors currPieces as nextSet
              in (val + 1, newSet)
          where nextSet = S.insert a set
