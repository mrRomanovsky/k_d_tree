module KDTree where
import Data.Array.IArray

type Node k = (Array Int) k

data KDNode k = KDNode {node :: Node k, splitsAt :: Int}
 deriving (Show, Eq)

data KDTree k = Empty | Fork (KDTree k) (KDNode k) (KDTree k)
 deriving (Show, Eq)