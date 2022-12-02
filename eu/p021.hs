isqrt :: Int -> Int
isqrt n = round $ sqrt (fromIntegral n)

proper_divisors n = 1:fc ++ [ div n x | x <- fc ]
    where fc = [ x | x <- [2 .. isqrt n], mod n x == 0 ]

is_amicable n
    | friend == n = False
    | otherwise = sum_friend == n
    where friend = sum $ proper_divisors n
          sum_friend = (sum $ proper_divisors friend)

func = sum [ i | i <- [1..10000], is_amicable i ]

main = do
    print func
