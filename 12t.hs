is_prime :: Int -> Bool
is_prime x
    | x == 2 = True
    | (mod x 2) == 0 = False
    | otherwise = pp 3
    where pp n
            | (round (sqrt (fromIntegral x))) < n = True
            | (mod x n) == 0 = False
            | otherwise = pp (n + 2)

rmdups :: Eq a => [a] -> [a]
rmdups = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

divisors n = (filter (\x -> mod n x == 0) [1 .. div n 2]) ++ [n]

remove_element _ [] = []
remove_element y (x:xs)
    | x == y = y:xs
    | otherwise = x:remove_element y xs

prime_factor :: Int -> [Int] -> [Int]
prime_factor n all@(x:xs)
    | n == 1 = []
    | x == 1 = x:prime_factor n xs
    | n `mod` x == 0 = x:prime_factor (n `div` x) all
    | otherwise = prime_factor n xs

triangle n = div ((n + 1) * n) 2

primes = [ x | x <- [1..1000000], is_prime x ]
-- factor_triangle a b = if an then (la - 1) * lb else la * (lb - 1)
--     where an = mod a 2 == 0
--           la = (length $ prime_factor a primes)
--           lb = (length $ prime_factor b primes)
-- func = maximum [ factor_triangle x (x + 1) | x <- [1..1000] ]

-- factor_triangle a b = rmdups [ s | x <- g, y <- g, let s = x * y, s <= triangle (min a b)]
factor_triangle a b = rmdups [ s | x <- g, y <- g, let s = x * y, s < triangle (min a b) ]
    where ae = even a
          pa = if ae then remove_element 2 (prime_factor a primes) else prime_factor a primes
          pb = if ae then prime_factor b primes else remove_element 2 (prime_factor b primes)
          g = (pa ++ pb)

-- func = [ length (factor_triangle i (i + 1)) | i <- [1..1000] ]
a = 12
b = 13
-- func = [ (length $ divisors $ triangle i) - factor_triangle i (i + 1) | i <- [ 1..50 ] ]
func = [ factor_triangle i (i + 1) | i <- [ 1..50 ] ]
-- func = rmdups [ length (divisors $ triangle i) | i <- [1..1000] ]
-- func = factor_triangle  5

main = do
    print func
