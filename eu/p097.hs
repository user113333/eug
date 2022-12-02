non_mersenne_prime = 28433 * (2^7830457) + 1
func = read last_ten :: Int
    where n = show non_mersenne_prime
          last_ten = drop ((length n) - 10) n

main = do
    print func
