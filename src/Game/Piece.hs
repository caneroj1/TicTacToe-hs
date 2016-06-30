module Game.Piece
(
  Piece(..)
, emptyToMove
) where

data Piece = X Int Int | O Int Int | Empty Int Int

emptyToMove :: Bool -> Piece -> Piece
emptyToMove isX (Empty x y)
  | isX       = X x y
  | otherwise = O x y
emptyToMove _   piece = piece

instance Show Piece where
  show (X _ _)     = "X"
  show (O _ _)     = "O"
  show (Empty x y) = " "
