module Search where

import Prelude hiding (length)

import Data.Vector hiding (head, mapM_)
import Data.Maybe

import Control.Monad.ST
import Data.HashTable.ST.Cuckoo hiding (mapM_)

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
import Trans

data Evaluation = Evaluation {
  bestScoreE :: {-# UNPACK #-} !Int,
  bestMovesE :: ![Move] } deriving Show

instance NFData Evaluation where
  rnf (Evaluation !_ moves) = rnf (fmap unMove moves)

-- alphaBeta :: Int -> Int -> Move -> Board -> Int -> Evaluation
-- alphaBeta !alpha !beta !move !board !depth
--   | depth == 0 || isTerminal board = Evaluation (evaluate board) [move]
--   | otherwise = let Evaluation !bestScore !bestMoves = negaLoop alpha 0 (generateMoves board) (Evaluation (minBound + 2) []) in
--                 Evaluation bestScore (move : bestMoves)
--   where negaLoop :: Int -> Int -> Vector Move -> Evaluation -> Evaluation
--         negaLoop !prevAlpha !index !children prevEval@(Evaluation !prevScore _)
--           | prevScore < beta && index < length children = negaLoop newAlpha (index + 1) children newEval
--             where newEval = case negate newScore of
--                     v | v > prevScore -> Evaluation v newMoves
--                       | otherwise     -> prevEval
--                   (Evaluation newScore newMoves) = alphaBeta (-beta) (-newAlpha) (children ! index)
--                                                       (applyMove (children ! index) board) (depth - 1)
--                   newAlpha = max prevScore prevAlpha
--         negaLoop _ _ _ bestEval = bestEval

type TransTable s = HashTable s Board Trans

alphaBetaTT :: Int -> Int -> Move -> Board -> TransTable s -> Int -> ST s Evaluation
alphaBetaTT alpha beta move board ttable depth = do
  mayEntry <- lookup board ttable

alphaBeta :: Int -> Int -> Move -> Board -> Int -> ST s Evaluation
alphaBeta !alpha !beta !move !board !depth
  | depth == 0 || isTerminal board = pure (Evaluation (evaluate board) [move])
  | otherwise = do Evaluation !bestScore !bestMoves <- negaLoop alpha 0 (generateMoves board) (Evaluation (minBound + 2) [])
                   pure (Evaluation bestScore (move : bestMoves))
  where negaLoop !prevAlpha !index !children prevEval@(Evaluation !prevScore _)
          | prevScore < beta && index < length children = do
             let !curMove  = children ! index
                 !newAlpha = max prevScore prevAlpha
             (Evaluation !newScore !newMoves) <- alphaBeta (-beta) (-newAlpha) curMove (applyMove curMove board) (depth - 1)
             let !newEval  = case negate newScore of
                   v | v > prevScore -> Evaluation v newMoves
                     | otherwise     -> prevEval
             negaLoop newAlpha (index + 1) children newEval
          | otherwise = pure prevEval

search alpha beta move board depth = runST (alphaBeta alpha beta move board depth)

iterativeDeepening :: Int -> Int -> Board -> Evaluation -> IO (Maybe Evaluation)
iterativeDeepening maxDepth time board startEval = listToMaybe <$> timeLimited time evals
 where evals = fmap (search (minBound + 1) maxBound (Move 0) board) [1..maxDepth]

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

