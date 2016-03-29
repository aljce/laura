module Board where

import Data.Word

import Data.Vector
import Data.HashMap.Strict

import BitBoard
import Move.Types
import Magic

type Turn = Bool

pattern Red   = False
pattern Black = True

data Board = Board {
  turnB     :: Turn,
  bitBoardB :: BitBoard,
  allMagics :: AllMagics,
  pv        :: Vector Move,
  transpoistions :: HashMap Word64 ()
}

instance Show Board where
  show = show . bitBoardB
