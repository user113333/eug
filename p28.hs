-- returns (k, suma)
calc_string n s = (s + 4 * n, 4 * s - 4 + 10 * n)

start 1003 _ = 1
-- start 7 _ = 1
start n s = suma + start (n + 2) k
    where (k, suma) = calc_string (n - 1) s

func = start 3 2

main = do
    print func