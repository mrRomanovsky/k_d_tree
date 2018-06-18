module KDZipper (Zipper, goUp, goLeft, goRight, goTop, History(LeftChild, RightChild)) where
import Prelude hiding (length)
import Data.Vector
import KDTree

data History k = LeftChild (Node k) (KDTree k) | RightChild (Node k) (KDTree k)

type KDTreeIdx k = (KDTree k, Int)

type Zipper k = (KDTreeIdx k, [History k])

goUp :: Zipper k -> Zipper k
goUp ((t, i), LeftChild p r : fs)  = ((Fork t p r, (i - 1) `mod` length p), fs)
goUp ((t, i), RightChild p l : fs) = ((Fork l p t, (i - 1) `mod` length p), fs)

goLeft :: Zipper k -> Zipper k
goLeft ((Fork l x r, i), fs) = ((l, (i + 1) `mod` length x), LeftChild x r : fs)

goRight :: Zipper k -> Zipper k
goRight ((Fork l x r, i), fs) = ((r, (i + 1) `mod` length x), RightChild x l : fs)

goTop :: Zipper k -> Zipper k
goTop (t, []) = (t, [])
goTop z = goTop $ goUp z