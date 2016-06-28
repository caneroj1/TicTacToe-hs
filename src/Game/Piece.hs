module Game.Piece
(
  Piece(..)
, Player(..)
) where

data Piece = X Int Int | O Int Int | Empty Int Int
data Player = P1 | P2 deriving Eq

instance Show Piece where
  show (X _ _)     = "X"
  show (O _ _)     = "O"
  show (Empty x y) = " "
