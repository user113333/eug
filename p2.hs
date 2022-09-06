fib :: [Integer] -> Integer -> [Integer]
fib arr@(a:b:_) l
    | n > l = arr
    | otherwise = fib (n:arr) l
    where n = a + b

fib3 :: [Integer] -> Integer -> [Integer]
fib3 arr@(a:b:_) l
    | n > l = arr
    | otherwise = fib3 (n:arr) l
    where n = 4 * a + b

-- solution 1
func = sum [ x | x <- (fib [1, 0] 4000000), even x ]

-- solution 2
-- func = sum (fib3 [8, 2] 4000000)

main = do
    print func
