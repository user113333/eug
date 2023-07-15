myButLast (x:_:[]) = x
myButLast (_:_:xs) = myButLast xs

main = do
    print $ myButLast [1,2,3,4]
    print $ myButLast ['a'..'z']
