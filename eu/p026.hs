import Data.List

decimals n d
    | next_n == 0 = []
    | otherwise = (n, dec_num):decimals next_n d
    where (dec_num, next_n) = divMod (n * 10) d

rec_cycle [] _ = 0
rec_cycle ((n, _):xs) leftovers
    | elem n leftovers = length $ dropWhile (==n) leftovers
    | otherwise = rec_cycle xs (n:leftovers)

rec_cycle_len n = rec_cycle (decimals 1 n) []

func = snd $ maximum [ (rec_cycle_len i, i) | i <- [1..999] ]

-- 983 -> 0m0.569s
main = do
    print func
