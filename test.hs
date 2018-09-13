
main = do
  text <- readFile "in.txt"
  let number = map (\x -> cast x) $ map (words) (lines text)
    in print $ pndAvg number

cast (x:[y]) = ((read x::Float),read y::Float)
pndAvg n = (foldl (\c (n,w) -> (n*w)+c) 0 n)/foldl (\c (_,w) -> w+c) 0 n
