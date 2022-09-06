prime :: Integer -> Integer -> Bool
prime n i
    | fromIntegral i > sqrt(fromIntegral n) = True
    |
    | mod n i == 0 = False
    | otherwise = prime n (i + 1)

is_prime a = prime a 2

get_primes a c i
    | length a == c = a
    | is_prime i = get_primes (i:a) c (i + 1)
    | otherwise = get_primes a c (i + 1)

get_prime a = head (get_primes [] a 2)

func = get_prime 10001

main = do
    print func
