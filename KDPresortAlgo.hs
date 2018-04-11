import KDTree
import KDAlgo
import Data.List hiding (partition)

--[k] is used instead of "Node k"
getSorts :: Ord k => Int -> [[k]] -> [[[k]]]
getSorts d src = sortRotate <$> [0..(d-1)]
  where
    sortRotate i = sortOn (rotate i) src

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

--один шаг алгоритма:
--из заданного списка узлов выбрать медиану
--разделить все остальные списки узлов пополам - меньше или больше этой медианы соответственно
--(по тому же самому ключу!)
--во всех списках этот узел должен оказаться по середине(а ещё он должен стать узлом дерева?)
--дополнительно передаём количество измерений, чтобы не нужно было каждый раз искать length l
step :: Ord k => [[[k]]] -> Int -> Int -> [[[k]]]
step lsts dims lstNumber = let med = (lsts !! lstNumber) !! (dims `div` 2)
                               in partition lstNumber med <$> lsts --не очень эффективно - список, из которого мы достаём медиану, можно и не обрабатывать 


--нужно сделать partition каждого списка кроме того, из которого выбирается медиана! 
--сравнение нужно проводить в нужном измерении!
partition :: Ord k => Int -> [k] -> [[k]] -> [[k]]
partition dim node lst = let (lower, upper) = foldr f ([], []) lst
                             in lst --lower ++ (node : upper)
  where
    f x (l, g)
      | rotate dim x < node = (x : l, g)
      | rotate dim x > node = (l, x : g)
      | otherwise = (l, g)