{-# LANGUAGE TupleSections #-}
module BitBoard where

import Data.Bits
import Data.Word

import Data.IntMap hiding (filter)

import Text.PrettyPrint.ANSI.Leijen

data BitBoard = BitBoard {
  black :: {-# UNPACK #-} !Word64,
  red   :: {-# UNPACK #-} !Word64,
  both  :: {-# UNPACK #-} !Word64 }

instance Show BitBoard where
  show  = show . vsep . bitBoardToDoc

startingBitBoard = BitBoard 0 0 0

splitEveryN :: Int -> [a] -> [[a]]
splitEveryN delta = unfold 0
  where unfold n xs
          | n < delta * delta = take delta xs : unfold (n + delta) (drop delta xs)
          | otherwise         = []

bitBoardToDoc :: BitBoard -> [Doc]
bitBoardToDoc (BitBoard b r _) = docs
  where docs = (fmap (string . tail) . drop 2 . splitEveryN 8) bitBoardString
        bitBoardString  = (reverse . elems . unions . fmap toIntMap) bitBoards
        toIntMap (ps,c) = (fromDistinctAscList . fmap (,c) . filter (testBit ps)) [0..63]
        bitBoards       = [(b,'X'),(r,'O'),(-1,'.')]

