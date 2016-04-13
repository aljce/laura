module Trans where

data Type = Exact | LowerBound | UpperBound deriving (Show, Eq, Ord, Enum)

data Trans = Trans {
  depthT :: {-# UNPACK #-} !Int,
  typeT  :: !Type,
  valueT :: {-# UNPACK #-} !Int } deriving (Show)
