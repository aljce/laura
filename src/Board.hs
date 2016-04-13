module Board where

import Prelude hiding ((<$>))

import Text.PrettyPrint.ANSI.Leijen

import Data.Hashable

import BitBoard
import Magic

type Turn = Bool

pattern Red   = False
pattern Black = True

data Board = Board {
  turnB     :: Turn,
  bitBoardB :: BitBoard,
  allMagics :: AllMagics
}

instance Hashable Board where
  hashWithSalt s (Board _ (BitBoard b r _) _) = s `hashWithSalt` b `hashWithSalt` r

startingBoard :: IO Board
startingBoard = fmap (Board Red startingBitBoard) loadMagics

instance Show Board where
  show (Board turn bb _)  = show $ string "Turn:" <+> turnToDoc turn <$> prettyBitBoard bb
    where turnToDoc Red   = "Red"
          turnToDoc Black = "Black"
