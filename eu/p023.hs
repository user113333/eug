import Data.List

sqrti i = round $ sqrt (fromIntegral i)

proper_divisors :: Int -> [Int]
proper_divisors 1 = []
proper_divisors n = 1:fp ++ sp
    where fp = [i | i <- [2..(sqrti n)], mod n i == 0]
          sp = if (sqrti n)^2 == n then map (div n) (init fp) else map (div n) fp

is_deficient n = (sum $ proper_divisors n) < n
is_abundant n = (sum $ proper_divisors n) > n

abundants = [ i | i <- [1..28123], is_abundant i ]

is_sum_two_abundant n = (intersect (map (\x -> (n - x)) ab) ab) /= []
    where ab = takeWhile (<n) abundants

-- 4179871 -> 3m47.227s
func1 = sum [ i | i <- [1..28123], not $ is_sum_two_abundant i ]

main = do
    print func
