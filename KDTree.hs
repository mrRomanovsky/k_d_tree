{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module KDTree where

import Data.Vector

type Node k = Vector k

data KDTree k = Empty | Fork (KDTree k) (Node k) (KDTree k)
  deriving (Show, Eq)
-- deriving (Show, Eq, Generic, NFData)