palindrome :: Int -> Bool
palindrome a = (a == read (reverse (show a)))

func = maximum (map (\(x, y) -> x * y) [ (x, y) | x <- [999, 998 .. 0], y <- [999, 998 .. 0], palindrome (x * y) ])

main = do
    print func
