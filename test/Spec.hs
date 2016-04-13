import Criterion.Main
import Criterion.Types

import Search
import Magic
import Board
import BitBoard
import Move.Types

config = defaultConfig {
  reportFile = Just "/home/kyle/Programming/haskell/laura/resources/testOutput"
                       }

main :: IO ()
main = do
  mags <- loadMagics
  let apply f = f (minBound + 1) maxBound (Move 0) (Board Red startingBitBoard mags)
  defaultMainWith config [
    bgroup "Non ST" (fmap (\d -> bench (show d) (whnf (apply search) d)) [5..10])]
