
root = (Node 5 (Node 3 (Node 2 Empty Empty) (Node 4 Empty Empty)) (Node 7 (Node 6 Empty Empty) (Node 8 Empty Empty)))
root1 = Empty
root2 = (Node 5 (Node 3 (Node 2 Empty Empty) (Node 4 Empty Empty)) (Node 7 (Node 6 (Node 0 Empty Empty) Empty) (Node 8 Empty Empty)))

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

--Add nodes to binary tree
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

--Get in-order route of Tree
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node n tl td) = (inOrder tl)++[n]++(inOrder td)

--Test if tree is a binary search Tree
testBin :: (Ord a) => Tree a -> Bool
testBin root = testOrder (inOrder root)
testOrder :: (Ord a) => [a] -> Bool
testOrder [ ] = True
testOrder [x] = True
testOrder (x:xs) = (x <= head xs)&&(testOrder xs)



-- --Test if tree is binary Tree
-- testBin :: (Ord a) => Tree a -> Bool
-- testBin (Node _ Empty Empty) = True
-- testBin (Node n Empty (Node b tlb tdb))
--   | (b <= n) = (testBin tlb)&&(testBin tdb)
--   | otherwise = False
-- testBin (Node n (Node a tla tda) Empty)
--   | (a <= n) = (testBin tla)&&(testBin tda)
--   | otherwise = False
-- testBin (Node n (Node a tla tda) (Node b tlb tdb))
--   | (a <= n)&&(b >= n) = (testBin tla)&&(testBin tda)&&(testBin tlb)&&(testBin tdb)
--   | otherwise = False
