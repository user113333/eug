pp x n
    | (round (sqrt (fromIntegral x))) < n = True
    | (mod x n) == 0 = False
    | otherwise = pp x (n + 2)

is_prime x
    | x == 2 = True
    | (mod x 2) == 0 = False
    | otherwise = pp x 3
    -- | (mod x 3) == 0 = False
    -- | (mod x 3) == 0 = False
    -- | (mod x 3) == 0 = False
    -- | (mod x 3) == 0 = False
    -- | (mod x 3) == 0 = False
    -- | (mod x 3) == 0 = False

-- Solution 1 -> 15.164s
func = sum [x | x <- [2 .. 2000000], is_prime x]

-- [2, 3, ..]
sieve a c
    | 

sum_sieve n = (sieve (take n (repeat 2000000)) 0)

func2 = sum_sieve 2000000

main = do
    print func2
