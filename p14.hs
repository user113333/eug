chain :: Integer -> [Integer]
chain i
    | i == 1 = [i]
    | otherwise = i:chain(n)
    where n | even i = (div i 2) | otherwise = (3 * i + 1)

chain_inv i
    | i > 1000000 = [i]
    | otherwise = i:chain_inv(n)
    where n | (even i || i == 1) = i * 2 | otherwise = (div (i - 1) 3)

maximum_snd (x:[]) = x
maximum_snd ((f, s):xs) = if s > s2 then (f, s) else (f2, s2)
    where t = maximum_snd xs
          f2 = fst t
          s2 = snd t

-- func = maximum [ length $ chain i | i <- [1..1000000] ]
-- func = maximum_snd [ (i, length $ chain i) | i <- [1..96] ]
-- func = [ (i, length $ chain i) | i <- [1..17] ]
set = [ x | x <- [1..1000000] ]
func = fst $ maximum_snd [ (i, length $ chain i) | i <- set ]

-- start = 97

main = do
    print func
    -- let c = chain_inv start
    -- print c
    -- print $ chain (last c)
