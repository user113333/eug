import Data.List

chunksOf _ [] = []
chunksOf n l = (take n l):chunksOf n (drop n l)

rect0 n = neighbors
    where normal = chunksOf n [ x | x <- [1..(n * n)] ]
          zeros = (replicate (n + 3) 0) ++ (intercalate [0, 0] normal) ++ (replicate (n + 3) 0)
          calc x y = zeros !! ((n + 2) * y + x)
          neighbors_0 = [[calc (x + 1) (y), calc (x) (y + 1), calc (x + 2) (y + 1), calc (x + 1) (y + 2)] | y <- [0..(n-1)], x <- [0..(n-1)]]
          neighbors = map (filter (/= 0)) neighbors_0

rect2 n = [1..n*n]
rect n = zip (rect2 n) (rect0 n)

each_square (i, e) (es_i, es) = (es_i, t1 es)
    where t1 tes = t2 $ delete e tes
          t2 tes = if es_i == e then delete i tes else tes

each_chain (i, e) xs = map (each_square (i, e)) xs

each_option [] = []
each_option (x:xs)
    | (let (i, es) = last x in es == []) = each_option xs
    | otherwise = [ each_chain (hi, e) t | e <- hes ] ++ each_option xs
    where (hi, hes) = head x
          t = tail x

start l 0 = l
start l i = start (each_option l) (i - 1)

-- f(6) = 207408 -> 4m40.034s
func = length $ start [l] (length l)
    where l = (rect 4)

main = do
    print func
