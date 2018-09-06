import Data.Char

vogalMaisComum s = maxTup [a,e,i,o,u]
  where
    a = ('a', countItem 'a' s)
    e = ('e', countItem 'e' s)
    i = ('i', countItem 'i' s)
    o = ('o', countItem 'o' s)
    u = ('u', countItem 'u' s)

countItem _ [] = 0
countItem y (x:xs)
  | (toLower x == toLower y) = 1 + countItem y xs
  | otherwise = countItem y xs

maxTup t = foldl (\(x,y) (w,z) -> if y>z then (x,y) else (w,z)) h t
           where h = head t
