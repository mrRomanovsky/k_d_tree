import KDTree
import KDZipper
import TestUtils
import KDAlgo

nodes = [node3, node2, node1, node4]

tree = fromNodesList 3 nodes

n1 = find node1 tree
n2 = find node2 tree
n3 = find node3 tree
n4 = find node4 tree
n5 = find node5 tree

test_find = checkZipNode n1 node1 
         && checkZipNode n2 node2
         && checkZipNode n3 node3
         && checkZipNode n4 node4
         && checkZipEmpty n5

test_go_top = checkZipTree (goTop n1) tree
           && checkZipTree (goTop n2) tree
           && checkZipTree (goTop n3) tree
           && checkZipTree (goTop n4) tree
           && checkZipTree (goTop n5) tree

n4_deleted = Fork (Fork Empty node1 Empty) node3 (Fork Empty node2 Empty)
n3_deleted = Fork (Fork (Fork Empty node4 Empty) node1 Empty) node2 Empty
n2_deleted = Fork (Fork (Fork Empty node4 Empty) node1 Empty) node3 Empty
n1_deleted = Fork (Fork Empty node4 Empty) node3 (Fork Empty node2 Empty)

test_delete = checkZipTree (goTop $ delete n5) tree
           && checkZipTree (goTop $ delete n4) n4_deleted
           && checkZipTree (goTop $ delete n3) n3_deleted
           && checkZipTree (goTop $ delete n2) n2_deleted
           && checkZipTree (goTop $ delete n1) n1_deleted