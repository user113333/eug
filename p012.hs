perfect :: Integer -> Integer
perfect p = round ((q * (q + 1)) / 2)
    where q = 2^p - 1

sqrti i = round $ sqrt $ fromIntegral i

divisors n = length (filter (\x -> mod n x == 0) [1 .. sqrti n]) * 2
divisors_v n = (length (filter (\x -> mod n x == 0) [1 .. (sqrti n - 1)]) * 2) + (if mod n (sqrti n) == 0 then 1 else 0)
triangle n = round (((n + 1) / 2) * n)

first_triangle i
    | (divisors_v $ triangle i) > 500 = i
    | otherwise = first_triangle (i + 1)

-- func = maximum [ (x, d) | i <- [1..10000], let x = triangle i, let d = divisors x, d > 100 ]
func = triangle $ first_triangle 1

main = do
    print func
