
root = (Node 5 (Node 3 (Node 2 Empty Empty) (Node 4 Empty Empty)) (Node 7 (Node 6 Empty Empty) (Node 8 Empty Empty)))

--Add nodes to binary tree
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

addNode :: (Ord a) => Tree a -> a -> Tree a
addNode Empty new = (Node new Empty Empty)
addNode (Node n tl td) new
  | (new <= n) = (Node n (addNode tl new) td)
  | otherwise = (Node n tl (addNode td new))

--Search in binary Tree
findNode :: (Ord a) => Tree a -> a -> Bool
findNode Empty _ = False
findNode (Node n tl td) target
  | (n == target) = True
  | (target < n)  = findNode tl target
  | otherwise = findNode td target

--Test if tree is binary Tree
-- binTree (Node n tl td) = testTree tl td n
-- testTree
-- testTree (Node a tl td) Empty n
--   | (a <= n) = (testTree tl td)
--   | otherwise = False
-- testTree Empty (Node a tl td) n
--   | (a >= n) = (testTree tl td)
--   | otherwise = False
-- testTree (Node a tla tda) (Node b tlb tdb) n
--   | (a <= n) && (b >= n) = (testTree tla tda a) & (testTree tlb tdb b)
--   | otherwise = False
