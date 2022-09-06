perfect :: Integer -> Integer
perfect p = round ((q * (q + 1)) / 2)
    where q = 2^p - 1

triangle n = round (((n + 1) / 2) * n)

divisors n = (filter (\x -> mod n x == 0) [1 .. div n 2]) ++ [n]

-- func = a 1
-- func = [ i | i <- [1..10] ]

main = do
    print [ triangle i | i <- [1..100] ]
    print [ (i, ld) | i <- [1..100], let ld = length $ divisors $ i, ld > 6 ]
    print $ divisors (2^5 * 3)
