import KDAlgo
import KDTree
import Data.Array.IArray

nodes = [[1, 2, 3], [7, 8, 9], [4, 5, 6], [-1, -2, -3]]

nodes2 = [[1, 2, 3], [7, 8, 9], [4, 5, 6], [-1, -2, -3], [1, 2, 3], [1, 2, 3]]

nodes3 = [[1, 2, 3], [7, 8, 9], [4, 5, 6], [-1, -2, -3], [7, 8, 9], [4, 5, 6]]

tree_1 = fromNodesList 3 $ listArray (0, 2) <$> nodes

tree_2 = fromNodesList 3 $ listArray (0, 2) <$> nodes2

tree_3 = fromNodesList 3 $ listArray (0, 2) <$> nodes3

test_res = Fork (Fork Empty (array (0,2) [(0,-1),(1,-2),(2,-3)]) Empty)
  (array (0,2) [(0,1),(1,2),(2,3)])
  (Fork (Fork Empty (array (0,2) [(0,4),(1,5),(2,6)]) Empty)
  (array (0,2) [(0,7),(1,8),(2,9)]) Empty)

test1 = tree_1 == test_res

test2 = tree_2 == test_res

test3 = tree_3 == test_res