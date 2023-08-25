rotate xs n =
    let n0 = mod n (length xs)
        jrp (a, b) = b ++ a
     in jrp $ splitAt n0 xs

main = do
    print $ rotate ['a','b','c','d','e','f','g','h'] 3
    print $ rotate ['a','b','c','d','e','f','g','h'] (-2)
