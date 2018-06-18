import KDAlgo
import KDTree
import KDPresortAlgo
import Prelude
import Data.Vector hiding (length)
import Control.Monad
import TestUtils

nodes = [node1, node2, node3, node4]

nodes2 = [node1, node2, node3, node4, node1, node1]

nodes3 = [node1, node2, node3, node4, node2, node3]

tree_1 = fromNodesList 3 nodes

tree_2 = fromNodesList 3 nodes2

tree_3 = fromNodesList 3 nodes3

test1 = checkTree tree_1 3

test2 = checkTree tree_2 3

test3 = checkTree tree_3 3

test4 = tree_1 == tree_2 && tree_2 == tree_3

tests_unbalanced = test1 && test2 && test3 && test4

test_balanced = let tree_balanced = balancedFromList 3 tst_nodes
                in isBalanced tree_balanced $ length tst_nodes