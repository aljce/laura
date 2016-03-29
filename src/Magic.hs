{-# LANGUAGE DeriveGeneric #-}
module Magic where

import Prelude hiding (foldr, replicate, unzip, unzip3)

import Data.Bits
import Data.Word

import Data.Vector hiding (maximum,(++))
import qualified Data.Vector.Unboxed as U
import qualified Data.List as L

import System.Random
import Control.Monad.Random
import Control.Monad.Trans
import Control.Arrow
import Control.Monad (when)

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe

import Masks
import Serialize



occupancyPerms :: Vector Word64 -> Vector (Vector (Word64,Int))
occupancyPerms = fmap genPerms
  where genPerms :: Word64 -> Vector (Word64,Int)
        genPerms w = (transform &&& attack) <$> enumFromN 0 (2^popCount w)
          where transform :: Word64 -> Word64
                transform num = foldr setBits 0 . indexed . indexedOnly $ w
                  where setBits (numI,wI) word
                          | testBit num numI = setBit word wI
                          | otherwise        = word

                attack :: Word64 -> Int
                attack w = (maximum . fmap (L.length . L.takeWhile id) . L.tails . fmap (testBit w)) [0..63]

testMagic :: Vector Word64 -> Vector Int -> Int -> Word64 -> Maybe (Vector (Maybe Int))
testMagic occ attSet size mag = ifoldM addOcc (replicate (2^size) Nothing) occ
  where addOcc v i o = addAttSet (v ! index)
          where index = fromIntegral $ (o * mag) `shiftR` (64 - size)
                addAttSet (Just as)
                  | as == attSet ! i = Just v
                  | otherwise        = Nothing
                addAttSet Nothing = Just (v // [(index,Just (attSet ! i))])

--           A single line          Pop count of line 
genOne :: Vector (Word64,Int) -> Int -> IO (Word64,Vector Int,Int)
genOne occAtt size = getStdGen >>= evalRandT (randMagics 0)
  where randMagics i = do
          randMagic <- (\w1 w2 w3 -> w1 .&. w2 .&. w3) <$> getRandom <*> getRandom <*> getRandom
          let (occ,att) = unzip occAtt
          case testMagic occ att size randMagic of
            Just v -> do
              lift $ putStrLn "success"
              return (randMagic, fmap toScore v, 64 - size)
            Nothing -> do
              when (i `mod` 1000 == 0 && i /= 0) $ lift $ putStrLn $ show i ++ " possible bitmaps attempted"
              randMagics (i+1)
        toScore (Just score)
          | score < 4 = score ^ 3
          | otherwise = 100000
        toScore Nothing = 0

data Magics = Magics {
  masks  :: !(U.Vector Word64),
  magics :: !(U.Vector Word64),
  rows   :: !(Vector (U.Vector Int)),
  shifts :: !(U.Vector Int) } deriving (Generic)

instance FromJSON Magics where

instance ToJSON Magics where

data AllMagics = AllMagics {
  vert     :: Magics,
  horz     :: Magics,
  diag     :: Magics,
  antiDiag :: Magics } deriving (Generic)

instance FromJSON AllMagics where

instance ToJSON AllMagics where

genMagics :: Vector Word64 -> IO Magics
genMagics v = do
  (mags,rows,shifts) <- unzip3 <$> zipWithM genOne (occupancyPerms v) (fmap popCount v)
  return (Magics (convert v) (convert mags) (fmap convert rows) (convert shifts))

jsonPath :: FilePath
jsonPath = "/home/kyle/Programming/haskell/laura/resources/magics.json"

magicMain :: IO ()
magicMain = do
  vert <- genMagics vertMasks
  horz <- genMagics horzMasks
  diag <- genMagics diagMasks
  antiDiag <- genMagics antiDiagMasks
  B.writeFile jsonPath (encode (AllMagics vert horz diag antiDiag))

loadMagics :: IO AllMagics
loadMagics = do
  maybeMags <- decode <$> B.readFile jsonPath
  case maybeMags of
    Just mags -> return mags
    Nothing   -> fail "Magic numbers uninitialized, run the magic generation function for magic numbers."
