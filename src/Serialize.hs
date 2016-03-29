module Serialize where

import Data.Bits
import Data.Word

import Data.Vector
import qualified Data.Vector.Mutable as M

import Move.Types

{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Word64) -> Word64 -> Vector Word64 #-}
{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Int   ) -> Word64 -> Vector Int    #-}
{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Move  ) -> Word64 -> Vector Move   #-}
expandBitBoard :: (Bits a, Num a) => (a -> b) -> a -> Vector b
expandBitBoard f w = create $ M.new (popCount w) >>= go 0 w
        where go i b v
                | b == 0    = pure v
                | otherwise = do
                        M.write v i (f (b .&. negate b))
                        go (i+1) (b .&. (b - 1)) v

bitBoards :: Word64 -> Vector Word64
bitBoards = expandBitBoard id

indexedOnly :: Word64 -> Vector Int
indexedOnly = expandBitBoard countTrailingZeros

