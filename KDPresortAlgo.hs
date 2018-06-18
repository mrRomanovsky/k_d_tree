module KDPresortAlgo (balancedFromList) where
import Data.Vector
import KDTree
import Data.List hiding (take, zipWith, drop, cycle, length, (++), foldr, partition, tail)
import Prelude hiding (take, drop, (++), length, foldr, tail)

getSorts :: Ord k => Int -> [Node k] -> Vector (Vector (Node k))
getSorts d src = sortRotate <$> generate d id
  where
    sortRotate i = fromList $ sort $ rotate i <$> src

rotate :: Int -> Vector k -> Vector k
rotate n v = (drop n v) ++ (take n v)

buildTree :: Ord k => Vector (Vector (Node k)) -> Int -> Int -> KDTree k
buildTree l i c = let cur = l ! i
                  in case length cur of
  0 -> Empty
  1 -> Fork Empty (cur ! 0) Empty
  2 -> Fork (Fork Empty (cur ! 0) Empty)
       (cur ! 1) Empty
  3 -> Fork (Fork Empty (cur ! 0) Empty)
       (cur ! 1) (Fork Empty (cur ! 2) Empty)
  n -> let m = cur ! (n `div` 2)
           d = length m
           (pl, ph, _) = foldr parts (empty, empty, d - 1) l
           parts xs (ls, hs, j) = let (lxs, hxs) =
                                        partition pf xs
                                      pf nd = let
                                                rc = (d - j + i) `mod` d
                                              in rotate rc nd < m 
                                  in (cons lxs ls,
                                     cons (tail hxs) hs, j - 1)
           next_idx = (i + 1) `mod` c
       in Fork (buildTree pl next_idx c) m (buildTree ph next_idx c)


balancedFromList :: (Ord k) => Int -> [Node k] -> KDTree k
balancedFromList d l = let sorts = getSorts d l
                           c     = length sorts 
                       in  buildTree sorts 0 c