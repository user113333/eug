loop l
    | last (take 2 l) == length l = l
    | otherwise = loop [ sum $ take i l | i <- [1..len] ]
    where len = length l

sum_sqtr s = last (loop $ replicate s 1)

func = sum_sqtr 21

main = do
    print func
