module Move.Generation where

import Prelude hiding (reverse)

import Data.Word
import Data.Bits

import Data.Vector.Unboxed
import qualified Data.Vector as V
import Data.Maybe

import BitBoard
import Board
import Move.Types
import Masks
import Serialize

moveWord :: Word64 -> Word64
moveWord w = complement (w .|. top2Rows) .&. ((w `shiftL` 8) .|. 127)
  where top2Rows = 9187061764859101184

generateMoves :: Board -> V.Vector Move
generateMoves (Board _ (BitBoard _ _ bth) _ _ _) = expandBitBoard Move $ moveWord bth

toMove :: Int -> BitBoard -> Maybe Move
toMove m (BitBoard _ _ bth)
  | (moves .&. vertLines ! (6 - m)) /= 0 = Just (Move (moves .&. vertLines ! (6 - m)))
  | otherwise                      = Nothing
  where moves = moveWord bth
        vertLines = convert vertMasks

unsafeToMove i = fromJust . toMove i

