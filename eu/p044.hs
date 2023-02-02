-- not even minimised D:

isInt x = x == (fromIntegral $ round x)
isPan x = isInt ((1 + sqrt ((24 * x) + 1)) / 6)
pan n = (n * (3 * n - 1)) / 2
pans = map pan [1..]
compi a b = isPan (a + b) && (isPan $ abs (a - b))

testl n = if r == [] then 0 else round $ abs (x - (head r))
    where
        r = dropWhile (\a -> not $ compi a x) xs
        x = head $ drop n pans
        xs = take n pans

func = head $ dropWhile (==0) $ map testl [1..]

main = print func
