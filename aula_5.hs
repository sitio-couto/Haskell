
mx = [[1,2,3],[4,5,6],[7,8,9],[0,0,-1]]
list = [1..10]
list2 = revertList list
list3 = [5,5,1,2,1,2,1,2,3,3,5]

-- listLength [] = 0
-- listLength (x:xs) = 1 + listLength xs

listLength l = foldl (\acc _ -> acc+1) 0 l

sumAll l = foldl (+) 0 l

sumPairIndex l = foldl (\acc (x,y) -> if mod x 2 == 0 then acc+y else acc) 0 $ zip [1..length l] l

isThere x l = [] /= filter (\y -> x==y) l

findPos i l = foldr (\(x,y) c -> if i==y then x:c else c) [] $ zip [1..length l] l

countItem i l = foldr (\x c -> if i == x then c+1 else c) 0 l

maxElem l = foldr (\x i -> if x > i then x else i) (head l) l

revertList l = foldl (\i x -> x:i) [] l

mergeList1 a b = foldr (\(x,y) i -> x:y:i) [] $ zip a b

shorterVec a b = if length a < length b then b else a
mergeList2 a b = (mergeList1 a b)++drop s l
                  where s = min (length a) (length b)
                        l = shorterVec a b

isSorted l = foldl (\i (x,y) -> if x<=y then i&&True else False) True $ pair l
pair l = zip l1 l2
         where l1 = init l
               l2 = tail l

dumpAll i l = foldr (\x c -> if x==i then c else x:c) [] l

changeAll o n l = foldr (\x c -> if x==o then n:c else x:c) [] l

t m = foldr (\x c -> [x]:c ) [] m

t1 a b = zipWith (\[a] b -> a:b) (t a) b

t2 m = foldr (\x c -> t2 x c) base m
       where base = replicate $ length m!!0 $ []
-- transpose2 m = foldr (\x c -> zipWith (\[a] b -> a:b) (transpose x) c) [] m


-- transpose m = foldl (\(i,m) mt -> mt:foldr (\x c -> (x!!i):c) [] m) [] $ zip [0..length m] m
