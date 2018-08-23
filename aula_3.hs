str = "abcdefghij"
str1 = "abcbcdcdefghabcdef"
list = [1..10]
list3 = [5,5,1,2,1,2,1,2,3,3,5]

posicoes y list = pos y list 1
  where pos _ [] _ = []
        pos y (x:xs) acc = [z | z <- [acc], x==y]++[w | w <- pos y xs (acc+1)]

-- split y list = splitRecur y list ""
--   where splitRecur _ [] _ = []
--         splitRecur y list acc = [ (a,b) | a <- l]
--
-- splitAll y list = splitAllRecur y list 0
--   where splitAllRecur _ [] _ = []
--         splitAllRecur y list acc = [ (x:y) | x <- list , acc == 0]
