-- (1, 0) == +1
-- on1 == O(n-1) -> next elem
into :: Int -> Int -> Int -> [(Int, Int)]
into n s on1 = [(1, 0)] ++ [ (on1, i * n) | i <- [1..(div s n)] ]

multi :: [(Int, Int)] -> Int -> [(Int, Int)]
multi l on1 = concat [ into n s on1 | (n, s) <- l ]

start :: [(Int, Int)] -> [Int] -> [(Int, Int)]
start l [] = l
start l (x:xs) = start (multi l x) xs
-- start l (x:y:xs) = start (multi l) y:xs

o = [ 10, 5, 2, 1 ]
size = 20
-- func = start [(size, head o)] o
-- func = multi [(1,0),(2,8),(2,16)] 1
-- func = start [(head o, size)] (tail o)

main = do
    print func
