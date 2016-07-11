module Game.Piece
(
  Piece(..)
, emptyToMove
, getX
, getY
) where

data Piece =  X Int Int
            | O Int Int
            | Empty Int Int deriving (Eq, Ord)

emptyToMove :: Bool -> Piece -> Piece
emptyToMove isX (Empty x y)
  | isX       = X x y
  | otherwise = O x y
emptyToMove _   piece = piece

getX (X xp _) = xp
getX (O xp _) = xp
getY (X _ yp) = yp
getY (O _ yp) = yp

instance Show Piece where
  show (X _ _)     = "X"
  show (O _ _)     = "O"
  show (Empty x y) = " "
