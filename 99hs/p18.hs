-- inclusive, counting starts at 1
slice xs i k = take (k - i + 1) (drop (i - 1) xs)

main = do
    print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
