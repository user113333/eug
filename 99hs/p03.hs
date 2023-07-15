elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)

main = do
    print $ elementAt [1,2,3] 2
    print $ elementAt "haskell" 5
