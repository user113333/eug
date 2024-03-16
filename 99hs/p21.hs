insertAt _ [] _ = []
insertAt c (x:xs) 1 = c:x:(insertAt c xs 0)
insertAt c (x:xs) i = x:(insertAt c xs (i-1))

main = do
    print $ insertAt 'X' "abcd" 2
