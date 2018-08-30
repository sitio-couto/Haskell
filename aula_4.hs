
root = (Node 5 (Node 3 (Node 2 Empty Empty) (Node 4 Empty Empty)) (Node 7 (Node 6 Empty Empty) (Node 8 Empty Empty)))
root1 = Empty
root2 = (Node 5 (Node 3 (Node 2 Empty Empty) (Node 4 Empty Empty)) (Node 7 (Node 6 (Node 0 Empty Empty) Empty) (Node 8 Empty Empty)))
a = makeTree [1..10]

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

--Add nodes to binary tree
addNode :: (Ord a) => Tree a -> a -> Tree a
addNode Empty new = (Node new Empty Empty)
addNode (Node n tl td) new
  | (new == n) = (Node n tl td)
  | (new < n) = (Node n (addNode tl new) td)
  | otherwise = (Node n tl (addNode td new))

--Search node to remove from binary Tree
removeNode :: (Ord a) => Tree a -> a -> Tree a
removeNode Empty _ = Empty
removeNode (Node n tl td) target
  | (target == n) = removeRoot tl td
  | (target < n) = (Node n (removeNode tl target) td)
  | otherwise = (Node n tl (removeNode td target))

--Removes root of tree and rearranges it
removeRoot :: (Ord a) => Tree a -> Tree a -> Tree a
removeRoot Empty Empty = Empty
removeRoot (Node n tl td) sister = (Node n (removeRoot tl td) sister)
removeRoot sister (Node n tl td) = (Node n sister (removeRoot tl td))

--Search in binary Tree
findNode :: (Ord a) => Tree a -> a -> Bool
findNode Empty _ = False
findNode (Node n tl td) target
  | (n == target) = True
  | (target < n)  = findNode tl target
  | otherwise = findNode td target

--Get inorder route of Tree
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node n tl td) = (inOrder tl)++[n]++(inOrder td)

--Get preorder route of tree
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node n tl td) = n:(preOrder tl)++(preOrder td)

--Test if tree is a binary search Tree
testBin :: (Ord a) => Tree a -> Bool
testBin root = testOrder (inOrder root)
testOrder :: (Ord a) => [a] -> Bool
testOrder [ ] = True
testOrder [x] = True
testOrder (x:xs) = (x <= head xs)&&(testOrder xs)

--Get tree height
getHeight :: (Ord a) => Tree a -> Int
getHeight Empty = -1
getHeight (Node n tl td) = 1 + max (getHeight tl) (getHeight td)

--Make binary tree from list
makeTree :: (Ord a) => [a] -> Tree a
makeTree [] = Empty
makeTree (x:xs) = addNode (makeTree xs) x

--Balance binary Tree
-- balanceTree :: (Ord a) => Tree a -> Tree a
-- rebuildTree :: (Ord a) => [a] -> Int -> Tree a
balanceTree root = let l = inOrder root in rebuildTree l (length l)
rebuildTree [] 0 = Empty
rebuildTree l s = (Node n tl td)
                  where n = l!!(quot s 2)
                        tl = rebuildTree (take (quot s 2) l) (quot s 2)
                        td = rebuildTree (drop (s-(quot s 2)) l) (s-(quot s 2)-1)

showSplits [] _ = []
showSplits l s = let p = quot s 2
                 in show ((take p l), (drop (s-p-1) l))



                        --
