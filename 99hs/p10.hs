pack [] = []
pack (x:xs) =
    let (a, b) = span (\y -> y == x) xs
     in [x:a] ++ pack b

encode = (map (\xs -> (length xs, head xs))) . pack

main = do
    print $ encode "aaaabccaadeeee"
