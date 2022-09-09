factorial 0 = 1
factorial i = i * factorial (i - 1)

sc [] = 0
sc (x:xs) = factorial (read [x]) + sc xs

func = [ i | i <- [10..100000], sc (show i) == i ]

main = do
    print func
