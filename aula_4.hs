
--Add nodes to binary tree
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

addNode Empty new = (Node new Empty Empty)
addNode (Node n tl td) new
  | (new <= n) = (Node n (addNode tl new) td)
  | otherwise = (Node n tl (addNode td new))
