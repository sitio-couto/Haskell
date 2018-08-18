str = "abcdefghij"
str1 = "abcbcdcdefghabcdef"
list = [1..10]
list3 = [5,5,1,2,1,2,1,2,3,3,5]

positions y list = getPos y list 1
getPos _ [] _ = []
getPos y (x:xs) acc
  | (y==x) = acc:getPos y xs (acc+1)
  | otherwise = getPos y xs (acc+1)

split y str = splitRecur y str ""
splitRecur _ "" acc = [reverse acc]
splitRecur y (x:xs) acc
  | (x==y) = [reverse acc]++[xs]
  | otherwise = splitRecur y xs (x:acc)

splitAll y str = splitAllRecur y str ""
splitAllRecur _ "" acc = [reverse acc]
splitAllRecur y (x:xs) acc
  | (x==y) = (reverse acc):(splitAllRecur y xs "")
  | otherwise = splitAllRecur y xs (x:acc)

dropElem 0 list = list
dropElem _ [] = []
dropElem n (x:xs) = dropElem (n-1) xs

takeElem n list = takeRecur n list []
takeRecur 0 _ acc = reverse acc
takeRecur _ [] _ = []
takeRecur n (x:xs) acc = takeRecur (n-1) xs (x:acc)
