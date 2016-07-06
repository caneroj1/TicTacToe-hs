module Main where

import Lib
import System.Exit (exitSuccess)

main :: IO ()
main = do
  (p1, p2) <- initGame
  gameLoop emptyBoard p1 p2
  where
    gameLoop board p1@Player{isX = x, decision = decision, isAI = ai} p2 = do
      outputBoard board
      if ai
        then runAI     x ai decision board p1 p2
        else runPlayer x ai decision board p1 p2

    runAI x ai decision board p1 p2 =
      let nextBoard = opponentMoveAB x board
        in do
          displayStatus ai decision nextBoard
          gameLoop nextBoard p2 p1

    runPlayer x ai decision board p1 p2 = do
      nextBoard <- getNextBoard x board
      displayStatus ai decision nextBoard
      gameLoop nextBoard p2 p1

    displayStatus isAI decision board
      | isAI && gameDone     = putStrLn "Defeat!"  >> finish
      | not isAI && gameDone = putStrLn "Victory!" >> finish
      | null $ moves board   = putStrLn "Draw!"    >> finish
      | otherwise            = return ()
      where gameDone = victory board decision

    finish = exitSuccess

    getNextBoard isX board
      | isX       = (`addPiece` board) <$> getInput X
      | otherwise = (`addPiece` board) <$> getInput O
