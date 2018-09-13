import Data.Char

str = "View fine me gone this name an rank. Compact greater and demands mrs the parlors. Park be fine easy am size away. Him and fine bred knew. At of hardly sister favour. As society explain country raising weather of. Sentiments nor everything off out uncommonly partiality bed.\n\nUp am intention on dependent questions oh elsewhere september. No betrayed pleasure possible jointure we in throwing. And can event rapid any shall woman green. Hope they dear who its bred. Smiling nothing affixed he carried it clothes calling he no. Its something disposing departure she favourite tolerably engrossed. Truth short folly court why she their balls. Excellence put unaffected reasonable mrs introduced conviction she. Nay particular delightful but unpleasant for uncommonly who. \n\nSame an quit most an. Admitting an mr disposing sportsmen. Tried on cause no spoil arise plate. Longer ladies valley get esteem use led six. Middletons resolution advantages expression themselves partiality so me at. West none hope if sing oh sent tell is. Started earnest brother believe an exposed so. Me he believing daughters if forfeited at furniture. Age again and stuff downs spoke. Late hour new nay able fat each sell. Nor themselves age introduced frequently use unsatiable devonshire get. They why quit gay cold rose deal park. One same they four did ask busy. Reserved opinions fat him nay position. Breakfast as zealously incommode do agreeable furniture. One too nay led fanny allow plate. "

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
