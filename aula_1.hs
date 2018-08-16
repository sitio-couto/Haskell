

list = [1..10]
list2 = revertList list
list3 = [5,5,1,2,1,2,1,2,3,3,5]

listLength [] = 0
listLength (x:xs) = 1 + listLength xs

sumAll [] = 0
sumAll (x:xs) = x + sumAll xs

sumPairIndex [] = 0
sumPairIndex [x] = 0
sumPairIndex (dump:x:xs) = x + sumPairIndex xs

isThere _ [] = False
isThere y (x:xs) = (x==y || isThere y xs)

findPos y l = findRecurse y l 0
findRecurse _ [] _ = 0
findRecurse y (x:xs) acc
  | (x==y) = acc + 1
  | otherwise = findRecurse y xs (acc+1)

countItem _ [] = 0
countItem y (x:xs)
  | (x==y) = 1 + countItem y xs
  | otherwise = countItem y xs

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
isSorted (x:xs) = (x<=head xs) && isSorted xs

createList 1 = [1]
createList n = createList (n-1) ++ [n]

getLast [x] = x
getLast (x:xs) = getLast xs

getInit [x] = []
getInit (x:xs) = x:getInit xs
