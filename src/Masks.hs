module Masks where

import Prelude hiding (foldr, take)
import qualified Prelude as P

import Data.Bits
import Data.Word

import Data.Vector
import qualified Data.List as L
-- import Data.Vector

makeLine :: Int -> Int -> Int -> Word64
makeLine start end delta = P.foldr ((.|.) . bit) 0 . P.take end $ start : gen
  where gen = L.unfoldr (\x -> Just (newX x,newX x)) start
          where newX x = delta + x

uncurryMakeLine :: (Int,Int,Int) -> Word64
uncurryMakeLine (s,e,d) = makeLine s e d

vertSet :: Vector (Int,Int,Int)
vertSet = fromList $ fmap (\x -> (x,6,8)) [0..6]

vertMasks :: Vector Word64
vertMasks = fmap uncurryMakeLine vertSet

horzSet :: Vector (Int,Int,Int)
horzSet = fromList $ fmap (\x -> (x * 8,7,1)) [0..5]

horzMasks :: Vector Word64
horzMasks = fmap uncurryMakeLine horzSet

diagSet :: Vector (Int,Int,Int)
diagSet = fromList $ P.zipWith (\s e -> (s,e,7)) [22,14,6,5,4,3] [4,5,6,6,5,4]

diagMasks :: Vector Word64
diagMasks = fmap uncurryMakeLine diagSet

antiDiagSet :: Vector (Int,Int,Int)
antiDiagSet = fromList $ P.zipWith (\s e -> (s,e,9)) [16,8,0,1,2,3] [4,5,6,6,5,4]

antiDiagMasks :: Vector Word64
antiDiagMasks = fmap uncurryMakeLine antiDiagSet
