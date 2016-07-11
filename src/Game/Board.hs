{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Board
(
  drawBoard
, emptyBoard
, addPiece
, moves
, getPieces
, victory
, allXs
, allOs
, piecesAtLoc
, maxColumnIndex
, maxRowIndex
, Loc(..)
, Orientation(..)
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

data Orientation = L | R
data Loc         = Row Int | Col Int | Diag Orientation

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
  where makeNewBoard = b // [((getX piece, getY piece), piece)]

moves :: Board -> [Piece]
moves = flip getPieces keepEmpties
  where
    keepEmpties = \case
        Empty _ _ -> True
        _         -> False

getPieces :: Board -> (Piece -> Bool) -> [Piece]
getPieces Board{getBoard = b} pfn = filter pfn $ elems b

victory :: Board -> (Piece -> Bool) -> Bool
victory Board{getBoard = b} pfn =
  checkRowsOrColumns b Row pfn ||
  checkRowsOrColumns b Col pfn ||
  checkDiagonals b pfn

allXs :: Piece -> Bool
allXs (X _ _) = True
allXs _       = False

allOs :: Piece -> Bool
allOs (O _ _) = True
allOs _       = False

piecesAtLoc :: Board -> Loc -> [Piece]
piecesAtLoc Board{..} = piecesAt getBoard

piecesAt :: BoardArray -> Loc -> [Piece]
piecesAt barr (Row row) = [barr ! (i, row) | i <- [0..maxColumnIndex]]
piecesAt barr (Col col) = [barr ! (col, i) | i <- [0..maxRowIndex]]
piecesAt barr (Diag L)  = [barr ! (i, i)   | i <- [0..maxRowIndex]]
piecesAt barr (Diag R)  = [barr ! (i, maxRowIndex - i) | i <- [0..maxRowIndex]]

checkRowsOrColumns :: BoardArray -> (Int -> Loc) -> (Piece -> Bool) -> Bool
checkRowsOrColumns barr locfn pfn = go locfn 0
  where
    go locfn pos
      | pos == totalRows                                    = False
      | all pfn $ piecesAt barr (locfn pos)                 = True
      | otherwise                                           = go locfn (pos+1)

checkDiagonals :: BoardArray -> (Piece -> Bool) -> Bool
checkDiagonals barr pfn = checkLeft || checkRight
  where
    checkLeft  = all pfn $ piecesAt barr (Diag L)
    checkRight = all pfn $ piecesAt barr (Diag R)
