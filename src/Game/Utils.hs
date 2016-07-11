module Game.Utils
(
  outputBoard
, getInput
, opponentMove
, opponentMoveAB
, initGame
) where

import Game.Board
import Game.Piece
import Game.Player
import AI.Minimax
import AI.AlphaBeta

import Control.Monad
import Data.Text (unpack, split, pack)
import System.Console.Haskeline
import Data.List (maximumBy)
import Data.Ord (comparing)
import System.Random (getStdGen, randomR)
import System.Exit (exitSuccess)
import Control.Applicative ((<$>))
import Data.Maybe (maybe)

outputBoard :: Board -> IO ()
outputBoard b = (putStrLn . unpack $ drawBoard b) >> putStrLn "\n"

getInput :: (Int -> Int -> Piece) -> IO Piece
getInput piece = do
  line <- runInputT defaultSettings $ getInputLine "Your move: "
  return $ processLn line
  where
    processLn Nothing   = undefined
    processLn (Just ln) = mkPiece
                        . map (read . unpack)
                        . split (== ',')
                        $ pack ln
    mkPiece [x, y] = piece x y

opponentMove :: Bool -> Board -> Board
opponentMove isX brd = addPiece (snd $ minimax brd Maximize isX) brd

opponentMoveAB :: Bool -> Board -> Board
opponentMoveAB isX brd = addPiece move brd
  where
    (s, move) = alphaBeta brd
                          Maximize
                          isX
                          (minBound :: Int)
                          (maxBound :: Int)
                          Nothing

initGame :: IO Players
initGame = do
  x   <- fmap (maybe True (== 'y')) $ runInputT defaultSettings
                                    $ getInputChar "X? (y/n): "
  num <- fmap (fst . choice) getStdGen
  let (me, ai) = (makePlayer x False, makePlayer (not x) True)
    in case num of
        1 -> putStrLn "You go first"  >> return (me, ai)
        2 -> putStrLn "You go second" >> return (ai, me)
  where choice      = randomR (1 :: Int, 2)
        makePlayer bEx ai
          | bEx       = Player { isX = bEx, isAI = ai, decision = allXs }
          | otherwise = Player { isX = bEx, isAI = ai, decision = allOs }
