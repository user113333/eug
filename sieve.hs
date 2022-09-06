divv m n
  | y == 0 = n : divv mm n
  | otherwise = []
  where (mm,y) = divMod m n




-- sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]
-- primes = sieve [2..]



sieve (x:xs) = x : (sieve [ y | y <- xs, mod y x > 0])
primes = 2 : (sieve [3,5..])



-- factorize n = concat $ map (divv n) (take 1000 primes)



fctrzWith n primes = concat $ map (divv n) primes

main = do
    print $ fctrzWith 28 primes
