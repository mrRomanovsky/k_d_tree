module TestUtils (checkTree, isBalanced, node1, node2, node3, node4, node5, tst_nodes, checkZipNode, checkZipEmpty, checkZipTree) where
import KDAlgo
import KDTree
import KDZipper
import Data.Vector hiding (length)

node1 = generate 3 (+1) :: Vector Int
node2 = generate 3 (+7) :: Vector Int
node3 = generate 3 (+4) :: Vector Int
node4 = Data.Vector.map negate node1 :: Vector Int
node5 = generate 3 (+100) :: Vector Int
nodes_lst = [[1, 2, 3], [4, 5, 6], [-1, -2, -3], [-4, -5, -6], [-7, -8, -9], [15, 16, 17], [22, 33, 22], [2, 4, 5], [222, 333, 222], [1, 2, 3], [4, 5, 6], [-1, -2, -3], [-4, -5, -6], [-7, -8, -9], [15, 16, 17], [22, 33, 22], [2, 4, 5], [111, 231, 442], [42, 33, 42], [256, 789, 11], [341, 0, -1], [25, 3, 24], [16, -10, 20], [30, -2, 17], [40, 8, 20], [23, 1412, 0], [55, 16, 24], [345, 1, 2],[2, 15, 1], [22, 34, 1], [512, 33, 4], [0, 13, 42], [56, 2, 17], [8, 1, 7], [234, 11, -29], [40, 20, 80], [441, 23, 42], [-5, -6, 18], [23, 0, -1], [42, 43, 44], [228, 415, 882], [504, 25, 345], [213, 456, 213], [0, 4, 0], [123, 42, 23], [123, 55, 16], [232, 0, 14]]
tst_nodes = Prelude.map fromList nodes_lst

treeHeight :: KDTree k -> Int
treeHeight Empty = 0
treeHeight (Fork l x r) = max (treeHeight l + 1) (treeHeight r + 1)

treeCorrect :: Ord k => KDTree k -> Int -> Int -> Bool
treeCorrect Empty _ _ = True
treeCorrect (Fork Empty x Empty) _ _ = True
treeCorrect (Fork Empty x rt@(Fork _ y _)) d s = x ! s <=  y ! s
                                                 && treeCorrect rt d ((s + 1) `mod` d)
treeCorrect (Fork lt@(Fork _ y _) x Empty) d s = x ! s > y ! s
                                                 && treeCorrect lt d ((s + 1) `mod` d)
treeCorrect (Fork lt@(Fork _ y _) x rt@(Fork _ z _)) d s = x ! s > y ! s
                                                 && x ! s <= z ! s
                                                 && treeCorrect lt d ((s + 1) `mod` d)
                                                 && treeCorrect rt d ((s + 1) `mod` d)

checkTree :: Ord k => KDTree k -> Int -> Bool
checkTree t d = treeCorrect t d 0

isBalanced :: KDTree k -> Int -> Bool
isBalanced t c = depth t < c * 5 

depth :: KDTree k -> Int
depth Empty = 0
depth (Fork l _ r) = 1 + max (depth l) (depth r)

checkZipNode :: Ord k => Zipper k -> Node k -> Bool
checkZipNode ((t, _), _) n = case t of
    Empty -> False
    (Fork l x r) -> x == n

checkZipEmpty :: Zipper k -> Bool
checkZipEmpty ((Empty, _), _) = True
checkZipEmpty _ = False

checkZipTree :: Ord k => Zipper k -> KDTree k -> Bool
checkZipTree ((tz, _), _) t = tz == t