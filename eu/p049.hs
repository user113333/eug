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

primes = [p | p <- [1000..9999], is_prime p]
ip n = elem n primes

is_perm [] [] = True
is_perm _ [] = False
is_perm l (x:xs) = is_perm (delete x l) xs

triple s i = (s, s + i, s + i * 2)
list_triple i = [t | s <- [1000..(9999 - i * 2)], let t = triple s i, triple_primes t]

triple_primes (e0, e1, e2) = (ip e0) && (ip e1) && (ip e2) && triple_perm (e0, e1, e2)
triple_perm (e0, e1, e2) = (is_perm (show e0) (show e1)) && (is_perm (show e0) (show e2)) && (is_perm (show e1) (show e2))

triple_concat (a, b, c) = (show a) ++ (show b) ++ (show c)

-- 296962999629 -> 3m29.774s
func = filter (\x -> x /= "148748178147") solutions
    where solutions = map triple_concat (concat [ list_triple i | i <- [1..5000] ])

main = do
    print func
