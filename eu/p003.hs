is_prime :: Int -> Int -> Bool
is_prime a i
    | (i == 1 || i == 0) = error "i must be 2"
    | (i >= round (sqrt(fromIntegral a))) = True
    | mod a i == 0 = False
    | otherwise = is_prime a (i + 1)

factor :: Int -> [Int]
factor a =
    let l = [ x | x <- [1.. round (sqrt (fromIntegral a))], (mod a x) == 0 ] in
    l ++ map (\x -> div a x) l

func = maximum [ x | x <- (factor 600851475143), is_prime x 2 ]

main = do
    print func
