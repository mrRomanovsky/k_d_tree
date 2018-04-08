{-# LANGUAGE OverloadedStrings #-}

import KDAlgo
import Control.Monad
import System.Random
import Data.Array.IArray
import System.IO
import Control.DeepSeq
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock
import KDTree

randomList :: Int -> Int -> Int -> IO [Int]
randomList a b c = getStdGen >>= return . take c . randomRs (a,b)

lstsToArrs :: (Ord k) => Int -> [[k]] -> [Array Int k]
lstsToArrs d = fmap $ listArray (0, d)

main = do
    nodes  <- force <$> lstsToArrs 5 <$> replicateM 10000000 (randomList (-100) 100 5)
    start <- getTime Monotonic
    let tree = force $ fromNodesList 5 nodes
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end