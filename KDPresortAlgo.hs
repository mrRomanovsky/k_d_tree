import KDTree
import KDAlgo
import Data.List

--[k] is used instead of "Node k"
getSorts :: Ord k => Int -> [[k]] -> [[[k]]]
getSorts d src = sortRotate <$> [0..(d-1)]
  where
    sortRotate i = sortOn (rotate i) src

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs