module Move.Generation where

import Prelude hiding (reverse)

import Data.Word
import Data.Bits

import Data.Vector.Unboxed hiding (foldr)
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Maybe

import BitBoard
import Board
import Move.Types
import Move.Apply
import Masks
import Magic
import Serialize

moveWord :: Word64 -> Word64
moveWord w = complement (w .|. top2Rows) .&. ((w `shiftL` 8) .|. 127)
  where top2Rows = 9187061764859101184

generateMoves :: Board -> V.Vector Move
generateMoves (Board _ (BitBoard _ _ bth) _) = expandBitBoard Move $ moveWord bth

toMove :: BitBoard -> Int -> Maybe Move
toMove (BitBoard _ _ bth) m
  | (moves .&. vertLines ! (6 - m)) /= 0 = Just (Move (moves .&. vertLines ! (6 - m)))
  | otherwise                      = Nothing
  where moves = moveWord bth
        vertLines = convert vertMasks

makeBoard :: AllMagics -> [Int] -> Maybe Board
makeBoard mags = L.foldl' mergeMoves (Just (Board Red startingBitBoard mags))
  where mergeMoves (Just b) m = flip applyMove b <$> toMove (bitBoardB b) m
        mergeMoves _ _ = Nothing

