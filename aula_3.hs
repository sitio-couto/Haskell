str = "abcdefghij"
str1 = "abcbcdcdefghabcdef"
list = [1..10]
list3 = [5,5,1,2,1,2,1,2,3,3,5]

posicoes y list = posRecur y list 1
  where posRecur _ [] _ = []
        posRecur y list acc = [z | z <- [1..length list], list!!(z-1) == y]

-- split y list = splitRecur y list ""
--   where splitRecur _ [] _ = []
--         splitRecur y list acc = [ (a,b) | a <- l]
--
-- splitAll y list = splitAllRecur y list 0
--   where splitAllRecur _ [] _ = []
--         splitAllRecur y list acc = [ (x:y) | x <- list , acc == 0]
