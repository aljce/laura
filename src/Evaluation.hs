module Evaluation where

import Prelude hiding (sum)

import Data.Word
import Data.Bits

import Data.Vector.Unboxed
import qualified Data.Vector as V

import BitBoard
import Board
import Magic

sumOneMagic :: Magics -> Word64 -> Int
sumOneMagic (Magics masks magics rows shfts) w = sum (izipWith3 getNumInRow masks magics shfts)
  where getNumInRow i mask magic shift = let index = fromIntegral $ ((mask .&. w) * magic) `shiftR` shift in
          rows V.! i ! index

sumAllMagics :: AllMagics -> Word64 -> Int
sumAllMagics (AllMagics ver hor dia antiDia) w = sumOneMagic ver w + sumOneMagic hor w + sumOneMagic dia w + sumOneMagic antiDia w

evaluate :: Board -> Int
evaluate (Board Red (BitBoard b r _) mags) = sumAllMagics mags r - sumAllMagics mags b
evaluate (Board Black (BitBoard b r _) mags) = negate $ sumAllMagics mags r - sumAllMagics mags b
