dropEvery [] _ = []
dropEvery xs n = take (n - 1) xs ++ dropEvery (drop n xs) n

main = do
    print $ dropEvery "abcdefghik" 3
