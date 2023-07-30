pack [] = []
pack (x:xs) =
    let (a, b) = span (\y -> y == x) xs
     in [x:a] ++ pack b

main = do
    print $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
