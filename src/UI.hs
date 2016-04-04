module UI where

import System.Console.Haskeline
import Control.Monad.Trans

import Control.Concurrent

import Safe

import Magic
import Board
import BitBoard
import Move.Types
import Move.Generation
import Move.Apply
import Evaluation
import Search

getTurn :: InputT IO Turn
getTurn = do
  minput <- getInputLine "Turn: "
  case minput of
    Nothing -> defaulting
    Just "" -> defaulting
    Just "Red" -> return Red
    Just "Black" -> return Black
    _ -> do
      outputStrLn "Please input Red or Black"
      getTurn
  where defaulting = do
          outputStrLn "Defaulting Red to move"
          return Red

getMaxDepth :: InputT IO Int
getMaxDepth = do
  minput <- getInputLine "Max Search Depth: "
  case minput of
    Nothing -> defaulting
    Just "" -> defaulting
    Just str -> case readMay str of
      Nothing -> do
        outputStrLn "No parse, try again."
        getMaxDepth
      Just depth | 0 < depth -> return depth
      _ -> do
        outputStrLn "Search Depth must be larger than 0"
        getMaxDepth
  where defaulting = do
          outputStrLn "Defaulting to positive infinity"
          return maxBound

getDelay :: InputT IO Int
getDelay = do
  minput <- getInputLine "Max Time to Search: "
  case minput of
    Nothing -> defaulting
    Just "" -> defaulting
    Just str -> case readMay str of
      Nothing -> do
        outputStrLn "No parse, try again."
        getDelay
      Just depth | 0 < depth -> return (depth * 1000000)
      _ -> do
        outputStrLn "Search time must be larger than 0"
        getDelay
  where defaulting = do
          outputStrLn "Defaulting to 10 seconds"
          return 10000

getMove :: BitBoard -> InputT IO (Maybe Move)
getMove bb = do
  minput <- getInputLine "Move: "
  case minput of
    Nothing -> do
      outputStrLn "Please input a move in the form 0-6"
      getMove bb
    Just str | str `elem` quitWords -> return Nothing
    Just str -> case readMay str of
      Nothing -> do
        outputStrLn "No parse, please input a move in the form 0-6"
        getMove bb
      Just move | 0 <= move && move <= 6 -> case toMove bb move of
                    Nothing -> do
                      outputStrLn "Outside board range, please input another candidate move"
                      getMove bb
                    Just checkedMove -> return (Just checkedMove)
      Just _ -> do
        outputStrLn "Please input a move in the range 0-6"
        getMove bb
  where quitWords = ["quit","Quit","Q","q"]

terminate :: InputT IO ()
terminate = outputStrLn "Thanks for playing!"

redLoop :: Int -> Int -> Board -> Evaluation -> InputT IO ()
redLoop maxDepth delay board eval = do
  move <- getMove (bitBoardB board)
  case move of
    Nothing -> terminate
    Just checkedMove -> do
      let playerB = applyMove checkedMove board
      outputStrLn $ show playerB
      newEval <- liftIO $ iterativeDeepening maxDepth delay playerB eval
      case newEval of
        Just checkedEval -> case bestMoveE checkedEval of
          (_:aiB:_) -> do
            outputStrLn ""
            outputStrLn $ show aiB
            outputStrLn $ "Score: " ++ show (bestScoreE checkedEval)
            outputStrLn $ "Depth: " ++ show (length (bestMoveE checkedEval) - 1)
            redLoop maxDepth delay aiB checkedEval
          _ -> do
            outputStrLn "Game Over!"
            terminate
        Nothing -> outputStrLn "AI unable to calculate, AI forfits."

blackLoop = undefined

gameLoop :: InputT IO ()
gameLoop = do
  mags <- liftIO loadMagics
  turn <- getTurn
  maxDepth <- getMaxDepth
  delay    <- getDelay
  let board = Board turn startingBitBoard mags
  case turn of
    Red   -> redLoop maxDepth delay board (Evaluation 0 [])
    Black -> blackLoop maxDepth delay board

mainUI = runInputT defaultSettings gameLoop
