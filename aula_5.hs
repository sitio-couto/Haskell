

list = [1..10]
list2 = revertList list
list3 = [5,5,1,2,1,2,1,2,3,3,5]

-- listLength [] = 0
-- listLength (x:xs) = 1 + listLength xs

listLength = foldl (\acc _ -> acc+1) 0

sumAll l = foldl (+) 0 l

sumPairIndex l = foldl (\acc (x,y) -> if mod x 2 == 0 then acc+y else acc) 0 (zip [1..length l] l)

isThere x l = [] /= filter (\y -> x==y) l

-- findPos i l = foldr (\(x,y) i -> if i==y then x else i) i (zip [1..length l] l)

countItem i l = foldr (\x i-> i+x) 0 (zipWith (\x y -> if x==y then 1 else 0) bin)
                where b = replicate (length l) i) l

maxElem l = foldr (\x i -> if x > i then x else i) (head l) l

revertList l = foldl (\i x -> x:i) [] l

mergeList1 a b = foldr (\(x,y) i -> x:y:i) [] (zip a b)

shorterVec a b = if length a < length b then b else a
mergeList2 a b = (foldr (\(x,y) i -> x:y:i) [] (zip a b))++drop s l
                  where s = min (length a) (length b)
                        l = shorterVec a b

isSorted l = foldl (\i (x,y) -> if x<=y then i&&True else False) True (pair l)
pair l = zip l1 l2
         where l1 = init l
               l2 = tail l

breakLeft 0 _ = []
breakLeft _ [] = []
breakLeft n (x:xs) = x:breakLeft (n-1) xs

breakRight 0 l = l
breakRight _ [] = []
breakRight n (x:xs) = breakRight (n-1) xs

shiftln n l = let
  y = mod n (listLength l)
  in breakRight y l ++ breakLeft y l

shiftrn n l = let
  leng = listLength l
  y = 1 + mod (leng - (n + 1)) leng
  in breakRight y l ++ breakLeft y l

dumpOne _ [] = []
dumpOne y (x:xs)
  | (x==y) = xs
  | otherwise = x:(dumpOne y xs)

dumpAll _ [] = []
dumpAll y (x:xs)
  | (x==y) = dumpAll y xs
  | otherwise = x:(dumpAll y xs)

dumpNs 0 _ l = l
dumpNs _ _ [] = []
dumpNs n y (x:xs)
  | (x==y) = dumpNs (n-1) y xs
  | otherwise = x:dumpNs n y xs

dumpLast y list = revertList (dumpOne y (revertList list))

changeOne _ _ [] = []
changeOne old new (x:xs)
  | (x==old) = new:xs
  | otherwise = x:changeOne old new xs

changeAll _ _ [] = []
changeAll old new (x:xs)
  | (x==old) = new:changeAll old new xs
  | otherwise = x:changeAll old new xs

changeNs 0 _ _ l = l
changeNs _ _ _ [] = []
changeNs n old new (x:xs)
  | (x==old) = new:changeNs (n-1) old new xs
  | otherwise = x:changeNs n old new xs
