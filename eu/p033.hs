import Data.List

cmp (a, b, c, d) = a * d == c * b

perms0 = [(a * 10 + b, c * 10 + d, b, d) |
    a <- [1..9],
    b <- [0..9],
    c <- [1..9],
    d <- [0..9],
    a == c, b /= 0 || d /= 0]

perms1 = [(a * 10 + b, c * 10 + d, b, c) |
    a <- [1..9],
    b <- [0..9],
    c <- [1..9],
    d <- [0..9],
    a == d, b /= 0 || d /= 0]

perms2 = [(a * 10 + b, c * 10 + d, a, d) |
    a <- [1..9],
    b <- [0..9],
    c <- [1..9],
    d <- [0..9],
    b == c, b /= 0 || d /= 0]

perms3 = [(a * 10 + b, c * 10 + d, a, c) |
    a <- [1..9],
    b <- [0..9],
    c <- [1..9],
    d <- [0..9],
    b == d, b /= 0 || d /= 0]

lowest_terms (a, b) = (a `div` c, b `div` c)
    where
        c = gcd a b

perms = perms0 ++ perms1 ++ perms2 ++ perms3
fperms = filter (\(a, b, _, _) -> a /= b) $ filter cmp perms
pairs = nub $ map (\(a, b, _, _) -> (min a b, max a b)) fperms
product_pairs = foldl1 (\(a, b) (c, d) -> (a * c, b * d)) pairs
func = snd $ lowest_terms product_pairs

main = do
    print func
