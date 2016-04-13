module Terminal where

import Prelude hiding (any,zipWith4)

import Data.Word
import Data.Bits

import Data.Vector.Unboxed
import qualified Data.Vector as V

import BitBoard
import Board
import Magic
import Move.Generation

testDir :: Magics -> Word64 -> Bool
testDir (Magics masks magics rows shfts) w = any (90000 <=) (izipWith3 getNumInRow masks magics shfts)
  where getNumInRow i mask magic shift = let index = fromIntegral $ ((mask .&. w) * magic) `shiftR` shift in
          rows V.! i ! index

isWin :: Turn -> BitBoard -> AllMagics -> Bool
isWin Black (BitBoard _ r _) (AllMagics ver hor dia antiDia) = testDir ver r || testDir hor r || testDir dia r || testDir antiDia r
isWin Red   (BitBoard b _ _) (AllMagics ver hor dia antiDia) = testDir ver b || testDir hor b || testDir dia b || testDir antiDia b

isWin' _ (BitBoard b r _) (AllMagics ver hor dia antiDia) = testDir ver r || testDir hor r || testDir dia r || testDir antiDia r || testDir ver b || testDir hor b || testDir dia b || testDir antiDia b

isTerminal :: Board -> Bool
isTerminal (Board t b@(BitBoard _ _ bth) mags) = moveWord bth == 0 || isWin' t b mags

