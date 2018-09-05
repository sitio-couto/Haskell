

list = [1..10]
list2 = revertList list
list3 = [5,5,1,2,1,2,1,2,3,3,5]

-- listLength [] = 0
-- listLength (x:xs) = 1 + listLength xs

listLength = foldl (\acc _ -> acc+1) 0

sumAll l = foldl (+) 0 l

sumPairIndex l = foldl (\acc (x,y) -> if mod x 2 == 0 then acc+y else acc) 0 (zip [1..length l] l)

isThere x l = [] /= filter (\y -> x==y) l

findPos i l = foldl (\i (x,y) -> if i==y then x else i) i (zip [1..length l] l)

countItem _ [] = 0
countItem y (x:xs)
  | (x==y) = 1 + countItem y xs
  | otherwise = countItem y xs

-- maxElem x l = fold 3

maxElem [x] = x
maxElem (x:xs) = let
    y = maxElem xs
  in if (x > y) then x else y

revertList lo = revertRecurse lo []
revertRecurse [] acc = acc
revertRecurse (x:xs) acc = revertRecurse xs (x:acc)

mergeList1 xs [] = []
mergeList1 [] ys = []
mergeList1 (x:xs) (y:ys) = x:y:(mergeList1 xs ys)

mergeList2 xs [] = xs
mergeList2 [] ys = ys
mergeList2 (x:xs) (y:ys) = x:y:(mergeList2 xs ys)

isSorted [x] = True
isSorted (x:xs) = (x <= head xs) && isSorted xs

createList n = createRecurse n 1
createRecurse 1 acc = [acc]
createRecurse n acc = acc:(createRecurse (n-1) (acc+1))

getLast [x] = x
getLast (x:xs) = getLast xs

getInit [x] = []
getInit (x:xs) = x:getInit xs

shiftl (x:xs) = xs ++ [x]
shiftr l =  getLast l:getInit l

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
