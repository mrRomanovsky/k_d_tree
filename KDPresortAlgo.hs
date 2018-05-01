--import KDTree
--import KDAlgo
import Data.List

getSorts :: Ord k => Int -> [[k]] -> [[[k]]]
getSorts d src = sortRotate <$> [0..(d-1)]
  where
    sortRotate i = sortOn (rotate i) src

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

data Tree k = Empty | Fork (Tree k) [k] (Tree k)

buildTree :: Ord k => [[[k]]] -> Int -> Int -> Tree k
buildTree l i tl = case l !! i of
  []         -> Empty
  (x:[])     -> Fork Empty x Empty
  (x:y:[])   -> Fork (Fork Empty x Empty) y Empty
  (x:y:z:[]) -> Fork (Fork Empty x Empty) y (Fork Empty z Empty)
  ns         -> let m        = ns !! (length ns `div` 2)
                    (pl, ph) = foldr part_acc ([], []) l
                    part_acc xs (ls, hs) = let (lxs, (x:hxs)) = partition (<m) xs
                                               in (lxs : ls, hxs : hs)
                    next_idx = (i + 1) `mod` tl
                    in Fork (buildTree pl next_idx tl) m (buildTree ph next_idx tl) 