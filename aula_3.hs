str = "abcdefghij"
str1 = "abcbcdcdefghabcdef"
list = [1..10]
list3 = [5,5,1,2,1,2,1,2,3,3,5]

posicoes y list = pos y list 1
  where pos _ [] _ = []
        pos y (x:xs) acc = [z | z <- [acc], x==y]++[w | w <- pos y xs (acc+1)]

posNext _ [] = 0
posNext y (x:xs)
  | (x==y) = 0
  | otherwise = 1 + posNext y xs

mapl list = mapList list 0
mapList [] _ = []
mapList (x:xs) i = (i,x):mapList xs (i+1)

split y list= let l = mapl list in
              [[z | (i,z) <- l, let pos = posNext y list, i<pos],
               [w | (i,w) <- l, let pos = posNext y list, i>pos]]

splitAll y list = let l = mapl list in 

--
-- split y str = splitRecur y str ""
--   where splitRecur _ "" acc = [reverse acc]
--         splitRecur y (x:xs) acc
--           | (x==y) = [reverse acc]++[xs]
--           | otherwise = splitRecur y xs (x:acc)
