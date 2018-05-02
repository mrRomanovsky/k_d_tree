{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module KDTree where

import GHC.Generics (Generic)
import Data.Array.IArray
import Control.DeepSeq

type Node k = (Array Int) k

data KDTree k = Empty | Fork (KDTree k) (Node k) (KDTree k)
 deriving (Show, Eq, Generic, NFData)