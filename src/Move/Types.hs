module Move.Types where

import Data.Word
import Data.Bits

newtype Move = Move { unMove :: Word64 }

instance Show Move where
  show (Move col) = show $ 6 - (countTrailingZeros col `mod` 8)

