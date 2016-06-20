module Game.Piece
(
  Piece(..)
) where

data Piece = X Int Int | O Int Int | Empty Int Int

instance Show Piece where
  show (X _ _)     = "X"
  show (O _ _)     = "O"
  show (Empty _ _) = " "
