{-# LANGUAGE OverloadedStrings #-}

module Game.Board
(
  drawBoard
, emptyBoard
, addPiece
, Board
) where

import Data.Text (Text, pack)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Game.Piece

type Row      = [Piece]
newtype Board = Board { getBoard :: [Row] }

maxColumnIndex = 2
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
