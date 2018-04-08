{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module KDTree where

import GHC.Generics (Generic)
import Data.Array.IArray
import Control.DeepSeq

type Node k = (Array Int) k

data KDNode k = KDNode {node :: Node k, splitsAt :: Int}
 deriving (Show, Eq, Generic, NFData)

data KDTree k = Empty | Fork (KDTree k) (KDNode k) (KDTree k)
 deriving (Show, Eq, Generic, NFData)