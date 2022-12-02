factorial 0 = 1
factorial n = n * factorial (n - 1)

pt r c = div (factorial r) (factorial c * factorial (r - c))

func = length $ concat [ [ n | c <- [ 0..r ], let n = pt r c, n > 1000000 ] | r <- [0..100] ]

main = do
    print func
