module Move.Apply where

import Data.Word
import Data.Bits

import BitBoard
import Board
import Move.Types

applyMove :: Move -> Board -> Board
applyMove (Move m) (Board Red   (BitBoard b r bth) mags) = Board Black newbb mags
  where newbb = BitBoard (b .|. m) r (bth .|. m)
applyMove (Move m) (Board Black (BitBoard b r bth) mags) = Board Red   newbb mags
  where newbb = BitBoard b (r .|. m) (bth .|. m)
