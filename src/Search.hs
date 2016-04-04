module Search where

import Prelude hiding (length)

import Data.Vector hiding (head, mapM_)
import Data.Maybe

import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import System.Timeout

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

instance NFData Evaluation where
  rnf (Evaluation score moves) = ()

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

alphaBeta alpha beta depth board = Evaluation score moves
  where (moves,score) = alpha_beta alpha beta board depth


-- alphaBeta :: Int -> Int -> Int -> Board -> Evaluation
-- alphaBeta !alpha !beta !depth !board
--   | depth == 0 || isTerminal board = Evaluation (evaluate board) [board]
--   | otherwise = let Evaluation !bestScore !bestBoards = negaLevel alpha 0 newBoards (Evaluation (minBound + 2) [])
--                     newBoards = (fmap (flip applyMove board) (generateMoves board)) in
--                 Evaluation bestScore (board : bestBoards) 
--   where negaLevel :: Int -> Int -> Vector Board -> Evaluation -> Evaluation
--         negaLevel !prevAlpha !index !children prevEval@(Evaluation !prevScore _)
--           | prevScore < beta && index < length children = negaLevel newAlpha (index + 1) children newEval
--             where newEval = case - newScore of
--                     v | v > prevScore -> Evaluation v newMoves
--                       | otherwise     -> prevEval
--                   (Evaluation newScore newMoves) = alphaBeta (-beta) (-newAlpha) (depth - 1) (children ! index)
--                   newAlpha = max prevScore prevAlpha
--         negaLevel _ _ _ bestEval = bestEval

iterativeDeepening :: Int -> Int -> Board -> Evaluation -> IO (Maybe Evaluation)
iterativeDeepening maxDepth time board startEval = listToMaybe <$> timeLimited time evals
  where evals = fmap (\d -> alphaBeta (minBound + 1) maxBound d board) [1..maxDepth]

-- |Given a time limit (in microseconds) and a list, compute as many elements
--  of the list as possible within the time limit.
timeLimited :: (NFData a) => Int -> [a] -> IO [a]
timeLimited t xs = do
    v <- newTVarIO []
    timeout t (forceIntoTVar v xs)
    readTVarIO v

-- |Compute the elements of a list one by one, consing them onto the front
--  of a @TVar@ as they are computed. Note that the result list will be
--  in reverse order.
forceIntoTVar :: (NFData a) => TVar [a] -> [a] -> IO ()
forceIntoTVar v = mapM_ (forceCons v) 

-- |Force a pure value, and cons it onto the front of a list stored in a @TVar@.
forceCons :: (NFData a) => TVar [a] -> a -> IO ()
forceCons v x = x `deepseq` atomically $ modifyTVar2 v (x:)

-- |Modify the value of a transactional variable
modifyTVar2 :: TVar a -> (a -> a) -> STM ()
modifyTVar2 v f = readTVar v >>= writeTVar v . f
