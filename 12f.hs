is_prime :: Int -> Bool
is_prime x
    | x == 2 = True
    | (mod x 2) == 0 = False
    | otherwise = pp 3
    where pp n
            | (round (sqrt (fromIntegral x))) < n = True
            | (mod x n) == 0 = False
            | otherwise = pp (n + 2)

primes = [ x | x <- [1..1000000], is_prime x ]
prime_factor :: Int -> [Int] -> [Int]
prime_factor n all@(x:xs)
    | n == 1 = []
    | x == 1 = x:prime_factor n xs
    | n `mod` x == 0 = x:prime_factor (n `div` x) all
    | otherwise = prime_factor n xs

main = do
    print $ prime_factor 28 primes
