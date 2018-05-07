import KDAlgo
import KDTree
--import KDPresortAlgo
import Prelude hiding (map)
import Data.Vector
import System.Random
import Control.Monad

node1 = generate 3 (+1) :: Vector Int
node2 = generate 3 (+7) :: Vector Int
node3 = generate 3 (+4) :: Vector Int
node4 = map negate node1 :: Vector Int

nodes = [node1, node2, node3, node4]

nodes2 = [node1, node2, node3, node4, node1, node1]

nodes3 = [node1, node2, node3, node4, node2, node3]

tree_1 = fromNodesList 3 nodes

tree_2 = fromNodesList 3 nodes2

tree_3 = fromNodesList 3 nodes3

{--test_res = Fork (Fork Empty (array (0,2) [(0,-1),(1,-2),(2,-3)]) Empty)
  (array (0,2) [(0,1),(1,2),(2,3)])
  (Fork (Fork Empty (array (0,2) [(0,4),(1,5),(2,6)]) Empty)
  (array (0,2) [(0,7),(1,8),(2,9)]) Empty)--}

test1 = checkTree tree_1 3

test2 = checkTree tree_2 3

test3 = checkTree tree_3 3

test4 = tree_1 == tree_2 && tree_2 == tree_3

tests = test1 && test2 && test3 && test4

--Tests for building balanced tree

{--randomList :: Int -> Int -> IO [Int]
randomList a b = getStdGen >>= return . randomRs (a,b)

randomArr :: Int -> IO (Array Int Int)
randomArr cnt = randomList (-500) 500 >>= return . listArray (0, cnt - 1) . take cnt

test_balance = do
    --nodes_b <- forM [1..5] (\_ -> take 3 <$> randomList (-500) 500) --все списки оказываются одинаковыми!
    let t = balancedFromList 3 nodes
    print t
    print $ treeHeight t--}