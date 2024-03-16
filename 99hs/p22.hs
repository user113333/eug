range s e
    | s == e = [s]
    | otherwise = s:(range (s+1) e)

main = do
    print $ range 4 9
