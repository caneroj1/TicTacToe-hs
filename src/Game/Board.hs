{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Game.Board
(
  drawBoard
, emptyBoard
, addPiece
, moves
, victory
, allXs
, allOs
, Board
) where

import Data.Text (Text, pack)
import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)
import Game.Piece
import Data.Array

-- An array to represent the board
type BoardArray = Array (Int, Int) Piece

newtype Board = Board { getBoard :: BoardArray }

data Loc = Rows | Cols

maxColumnIndex = totalColumns - 1
maxRowIndex    = totalRows - 1
totalColumns   = 3
totalRows      = totalColumns

emptyBoard :: Board
emptyBoard = Board $ array ((0, 0), (maxColumnIndex, maxRowIndex))
                    [((x, y), Empty x y) | y <- [0..maxRowIndex],
                                           x <- [0..maxColumnIndex]]

drawBoard :: Board -> Text
drawBoard Board { getBoard = b } = pack $
  drawRow 0 ++ buffer ++ drawRow 1 ++ buffer ++ drawRow 2
  where drawRow r = intercalate "|" $
                    map show [b ! (i, r) | i <- [0..maxColumnIndex]]
        buffer    = "\n-+-+-\n"

addPiece :: Piece -> Board -> Board
addPiece p@(Empty _ _) _           = undefined -- FIXME
addPiece piece Board{getBoard = b} = Board makeNewBoard
  where makeNewBoard = b // [((x piece, y piece), piece)]
        x (X xp _) = xp
        x (O xp _) = xp
        y (X _ yp) = yp
        y (O _ yp) = yp

moves :: Board -> [Piece]
moves Board{getBoard = b} = keepEmpties $ elems b
  where
    keepEmpties =
      filter (\case
        Empty _ _ -> True
        _         -> False)

victory :: Board -> (Piece -> Bool) -> Bool
victory Board{getBoard = b} pfn =
  checkRowsOrColumns b Rows pfn ||
  checkRowsOrColumns b Cols pfn ||
  checkDiagonals b pfn

allXs :: Piece -> Bool
allXs (X _ _) = True
allXs _       = False

allOs :: Piece -> Bool
allOs (O _ _) = True
allOs _       = False

checkRowsOrColumns :: BoardArray -> Loc -> (Piece -> Bool) -> Bool
checkRowsOrColumns barr loc pfn = go loc 0
  where
    go loc pos
      | pos == totalRows                                    = False
      | all pfn $ pieces pos loc                            = True
      | otherwise                                           = go loc (pos+1)
    pieces pos Rows = [barr ! (i, pos) | i <- [0..maxColumnIndex]]
    pieces pos Cols = [barr ! (pos, i) | i <- [0..maxRowIndex]]

checkDiagonals :: BoardArray -> (Piece -> Bool) -> Bool
checkDiagonals barr pfn = checkLeft || checkRight
  where
    checkLeft  = all pfn [barr ! (i, i)               | i <- [0..maxRowIndex]]
    checkRight = all pfn [barr ! (i, maxRowIndex - i) | i <- [0..maxRowIndex]]
