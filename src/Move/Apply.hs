module Move.Apply where

import Data.Word
import Data.Bits

import BitBoard
import Board
import Move.Types

applyMove :: Move -> Board -> Board
applyMove (Move m) (Board Red   (BitBoard r b bth) mags pvs trans) = Board Black newbb mags pvs trans
  where newbb = BitBoard (r .|. m) b (bth .|. m)
applyMove (Move m) (Board Black (BitBoard r b bth) mags pvs trans) = Board Red   newbb mags pvs trans
  where newbb = BitBoard r (b .|. m) (bth .|. m)
