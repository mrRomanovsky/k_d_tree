module KDZipper where

import KDTree

data History k = LeftChild (KDNode k) (KDTree k) | RightChild (KDNode k) (KDTree k)

type Zipper k = (KDTree k, [History k])

goUp :: Zipper k -> Zipper k
goUp (t, LeftChild p r : fs) = (Fork t p r, fs)
goUp (t, RightChild p l : fs) = (Fork l p t, fs)

goLeft :: Zipper k -> Zipper k
goLeft (Fork l x r, fs) = (l, LeftChild x r : fs)

goRight :: Zipper k -> Zipper k
goRight (Fork l x r, fs) = (r, RightChild x l : fs)

goTop :: Zipper k -> Zipper k
goTop (t, []) = (t, [])
goTop z = goTop $ goUp z