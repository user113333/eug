-- fa n = 200 * n
-- fb n = 100 * n
-- fc n = 50 * n
-- fd n = 20 * n
-- fe n = 10 * n
-- ff n = 5 * n
-- fg n = 2 * n
-- fh n = 1 * n

-- func = length [ s | b <- [0..1], c <- [0..3], d <- [0..9], e <- [0..19], f <- [0..39], g <- [0..99], h <- [0..0], let s = (fb b + fc c + fd d + fe e + ff f + fg g + fh h), s == 200 ]

-- f5 s
--     | s < 4 = [(2, s)]
--     | otherwise = (2, s):(f5 (s - 5))

-- f10 s
--     | s < 10 = [(5, s)]
--     | otherwise = (5, s):(f10 (s - 10))

only_elem _ [] = True
only_elem e ((n, _):xs) = e == n && only_elem e xs

f_all current next size
    | size < current = [(next, size)]
    | otherwise = (next, size):(f_all current next (size - current))

f (n, s)
    | n == 1 = [(n, s)]
    | n == 2 = [(1, (div s 2) + 1)]
    | n == 5 = f_all 5 2 s
    | n == 10 = f_all 10 5 s
    | n == 20 = f_all 20 10 s
    | n == 50 = f_all 50 20 s
    | n == 100 = f_all 100 50 s
    | n == 200 = f_all 200 100 s

-- while not all n == 1
f_recursive conns
    | only_elem 1 conns = conns
    | otherwise = f_recursive $ concat [ f i | i <- conns ]

sum_sizes l = sum [ s | (_, s) <- l ]

-- first try = 69590
-- correct = 73682
func = sum_sizes $ f_recursive [(200, 1000)]

main = do
    print func
