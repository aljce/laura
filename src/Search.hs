module Search where

import Prelude hiding (length)

import Data.Vector hiding (head)
import Data.Function

import BitBoard
import Board
import Move.Types
import Move.Generation
import Move.Apply
import Evaluation
import Terminal
import Magic

data Evaluation = Evaluation {
  bestScoreE :: {-# UNPACK #-} !Int,
  bestMoveE  :: ![Board] } deriving Show


negamax :: Board -> Int -> Int -> Int -> Evaluation
negamax !b !depth !alpha !beta
  | depth == 0 || isTerminal b = Evaluation (evaluate b) [b]
  | otherwise = let (Evaluation !v !moves) = negaLevel undefined (minBound + 1) alpha 0 (generateMoves b) in
    Evaluation v (b : moves)
  where --negaLevel :: Move -> Int -> Int -> Int -> Vector Move -> Evaluation
        negaLevel curMove !curScore !a !index !moves
          | index < length moves && a < beta = let (Evaluation !v !newMove) =
                                                     negamax (applyMove (moves ! index) b) (depth - 1) (-beta) (-a)
                                                   bestMove = if curScore < v then newMove else curMove in
                                                     negaLevel bestMove (max curScore (-v)) (max a (-v)) (index + 1) moves
          | otherwise = Evaluation curScore curMove

alpha_beta :: Int -> Int -> Board -> Int -> ([Board],Int)
alpha_beta alpha beta node depth
    | isTerminal node || depth == 0 = ([node], evaluate node)
    | otherwise                      =  case fmap (flip applyMove node) (toList (generateMoves node)) of
                                          (c:cs) -> (node:pvm, pvv)
                                              where (pvm, pvv) = negaLevel ([], (minBound :: Int) + 2) alpha beta (c:cs)
    where negaLevel prev_best@(_, old_v) prev_alpha beta' (n:nn) | old_v < beta'
            = negaLevel best4 alpha' beta' nn
                where best4 = case neg $ alpha_beta (-beta') (-alpha') n (depth - 1) of
                                 value@(_, v) | (v > old_v) -> value
                                              | otherwise -> prev_best
                      alpha' = if old_v > prev_alpha then old_v else prev_alpha
          negaLevel best _ _ _     = best
          neg (m, v) = (m, -v)

alphaBeta :: Int -> Int -> Int -> Board -> Evaluation
alphaBeta !alpha !beta !depth !board
  | depth == 0 || isTerminal board = Evaluation (evaluate board) [board]
  | otherwise = let Evaluation !bestScore !bestBoards = negaLevel alpha 0 newBoards (Evaluation (minBound + 2) [])
                    newBoards = (fmap (flip applyMove board) (generateMoves board)) in
                Evaluation bestScore (board : bestBoards) 
  where negaLevel :: Int -> Int -> Vector Board -> Evaluation -> Evaluation
        negaLevel !prevAlpha !index !children prevEval@(Evaluation !prevScore _)
          | prevScore < beta && index < length children = negaLevel newAlpha (index + 1) children newEval
            where newEval = case - newScore of
                    v | v > prevScore -> Evaluation v newMoves
                      | otherwise     -> prevEval
                  (Evaluation newScore newMoves) = alphaBeta (-beta) (-newAlpha) (depth - 1) (children ! index)
                  newAlpha = max prevScore prevAlpha
        negaLevel _ _ _ bestEval = bestEval
{-
alphaBeta :: Int -> Int -> Int -> Board -> Move -> Evaluation
alphaBeta !depth !alpha !beta !b !curMove
  | depth == 0 || isTerminal b = Evaluation (evaluate b) [curMove]
  | otherwise = negaLevel undefined
  where negaLevel :: Int -> Int -> Int -> [Move] -> Evaluation
        negaLevel index a curScore curMove
          | index < length moves && a < beta = let (Evaluation !v !deeperMoves) =
                                                     alphaBeta (depth - 1) (-beta) (-a) (applyMove (moves ! index)) (moves ! index)
          where moves = generateMoves b
-}

searchMain = do
  mags <- loadMagics
  n    <- read <$> getLine
  loop (Board Black startingBitBoard mags undefined undefined) n
  where loop b n = do
          --let (Evaluation bestScore bestMove) = maximumBy (compare `on` bestScoreE) $ fmap (negamax b n (minBound + 1) maxBound) (generateMoves b)
          let Evaluation bestScore bestMoves = alphaBeta (minBound + 1) maxBound n b
          case Prelude.tail bestMoves of
            (x:xs) -> do
              let newB = x
              putStrLn $ "Score: " Prelude.++ show bestScore
              print newB
              (Just move) <- (\line -> toMove (read line) (bitBoardB newB)) <$> getLine
              loop (applyMove move newB) n
            [] -> do
              print b
              putStrLn "Game Over"
