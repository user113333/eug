sqrti :: Int -> Int
sqrti a = floor $ sqrt $ fromIntegral a

ando a b c = a c && b c

is_prime :: Int -> Bool
is_prime n = length ll == 0
        where ll = filter (\x -> ((mod n x) == 0)) [2..(round $ sqrt (fromIntegral n))]

is_prime_mem n = map is_prime [0..] !! n

goldbach_conjecture a = or $ map is_prime_mem $ map (\x -> a - (2 * x^2)) lx
    where lx = [1..sqrti (div a 2)]

func = head $ dropWhile goldbach_conjecture oc
    where oc = filter (ando odd (not . is_prime)) [1..]

main = do
    print func

