dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

main = do
    print $ dupli [1, 2, 3]
