repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

main = do
    print $ repli "abc" 3
