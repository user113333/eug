fib i a b
    | a > 10^999 = i
    | otherwise = fib (i+1) (a + b) a

main = do
    print $ fib 2 1 1