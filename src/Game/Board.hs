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

type Row      = [Piece]
newtype Board = Board { getBoard :: [Row] }

maxColumnIndex = totalColumns - 1
totalColumns   = 3

emptyBoard :: Board
emptyBoard = Board $ map makeRow [0..maxColumnIndex]
  where makeRow row = [Empty 0 row, Empty 1 row, Empty 2 row]

drawBoard :: Board -> Text
drawBoard Board { getBoard = [r1, r2, r3] } = pack $
  drawRow r1 ++ buffer ++ drawRow r2 ++ buffer ++ drawRow r3
  where drawRow = intercalate "|" . map show
        buffer  = "\n-+-+-\n"

addPiece :: Piece -> Board -> Board
addPiece p@(Empty _ _) Board{getBoard = b} = undefined
addPiece piece Board{getBoard = b} = makeNewBoard
  where makeNewBoard  = Board $ modify (concat b) (toIndex (x piece) (y piece))
        toIndex i j   = totalColumns * j + i
        modify ls idx = chunksOf totalColumns $ prefix ++ [piece] ++ suffix
          where prefix = take idx       ls
                suffix = drop (idx + 1) ls
        x (X xp _) = xp
        x (O xp _) = xp
        y (X _ yp) = yp
        y (O _ yp) = yp

moves :: Board -> [Piece]
moves Board{getBoard = b} = concatMap keepEmpties b
  where
    keepEmpties =
      filter (\case
        Empty _ _ -> True
        _         -> False)

victory :: Board -> (Piece -> Bool) -> Bool
victory Board{getBoard = b} pfn =
  checkRows b pfn ||
  checkRows (transpose b) pfn ||
  checkDiagonals b pfn

allXs :: Piece -> Bool
allXs (X _ _) = True
allXs _       = False

allOs :: Piece -> Bool
allOs (O _ _) = True
allOs _       = False

checkRows :: [Row] -> (Piece -> Bool) -> Bool
checkRows []     _   = False
checkRows (r:rs) pfn = all pfn r || checkRows rs pfn

checkDiagonals :: [Row] -> (Piece -> Bool) -> Bool
checkDiagonals rs pfn = checkLeft rs || checkRight rs
  where

    checkLeft [[l, _, _],
               [_, m, _],
               [_, _, r]]  = pfn l && pfn m && pfn r

    checkRight [[_, _, l],
                [_, m, _],
                [r, _, _]] = pfn l && pfn m && pfn r
