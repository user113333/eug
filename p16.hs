sumchar [] = 0
sumchar (x:xs) = read [x] + sumchar xs

func = sumchar $ show (2^1000)

main = do
    print func
