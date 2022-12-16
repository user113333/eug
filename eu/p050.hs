bellow = 1000000

is_prime n = go 2
    where
        go s
            | s * s > n = True
            | rem n s == 0 = False
            | otherwise = go (s + 1)

primes = filter is_prime [2..]

start i
    | primes !! i * 2 > bellow = []
    | otherwise = (length zps, head zps):(start (i + 1))
        where zps = dropWhile (not . is_prime) $ reverse $ takeWhile (<bellow) $ scanl1 (+) $ drop i primes

func = snd $ foldl1 (\(a0, b0) (a1, b1) -> if a1 > a0 then (a1, b1) else (a0, b0)) $ start 0
-- func = start 0

main = print func
