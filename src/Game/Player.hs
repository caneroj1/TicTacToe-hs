module Game.Player
(
  Player(..)
, Players
) where

import Game.Piece (Piece)

type Players = (Player, Player)

data Player =
  Player {
    isX      :: Bool
  , isAI     :: Bool
  , decision :: Piece -> Bool
  }
