import Data.List

pp x n
    | (round (sqrt (fromIntegral x))) < n = True
    | (mod x n) == 0 = False
    | otherwise = pp x (n + 2)

is_prime x
    | x < 0 = False
    | x == 2 = True
    | (mod x 2) == 0 = False
    | otherwise = pp x 3

find_primes :: Int -> (Int -> Int) -> Int
find_primes n f
    | is_prime (f n) = find_primes (n + 1) f
    | otherwise = n

bound = 1000

func = a * b
    where (a, b, _) = last $ sortBy (\(_, _, pn0) (_, _, pn1) -> compare pn0 pn1) [ (a, b, find_primes 0 (\x -> x^2 + a*x + b)) | a <- [(-bound)..bound], b <- [(-bound)..bound] ]

main = do
    print func
