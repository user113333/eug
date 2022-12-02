f20 i
    | ((l > 0) || (i == 0)) = f20 (i + 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19)
    | otherwise = i
    where r = filter (\x -> mod i x /= 0) [1..20]
          l = length r

func = f20 0

main = do
    print func
