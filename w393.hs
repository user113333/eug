import Data.List

chunksOf _ [] = []
chunksOf n l = (take n l):chunksOf n (drop n l)

subsets_c :: [[a]] -> [a] -> Int -> [[a]]
subsets_c [] _ _ = []
subsets_c l1@(x:xs) l2 n
    | n >= length l2 = subsets_c xs l2 0
    | otherwise = ((l2 !! n):x):(subsets_c l1 l2 (n + 1))

subsets :: [[a]] -> [a] -> [[a]]
subsets l1 l2 = subsets_c l1 l2 0

-- list to list of list
l2ll :: [a] -> [[a]]
l2ll l = map (\x -> [x]) l

combinations l = foldl subsets (l2ll $ head l) (tail l)

-- rect0 = [[2, 5], [1,6,3], [2,7,4], [3,8], [1,6,9], [2, 7, 10, 5], [3, 8, 11, 6], [4, 7, 12], [5, 10, 13], [6, 11, 14, 9], [7, 12, 15, 10], [8, 16, 11], [9, 14], [10, 15, 13], [11, 16, 14], [12, 15]]
-- rect2 = [1..16]
-- rect = zip rect2 rect0

rect0 n = neighbors
    where normal = chunksOf n [ x | x <- [1..(n * n)] ]
          zeros = (replicate (n + 3) 0) ++ (intercalate [0, 0] normal) ++ (replicate (n + 3) 0)
          calc x y = zeros !! ((n + 2) * y + x)
          neighbors_0 = [[calc (x + 1) (y), calc (x) (y + 1), calc (x + 2) (y + 1), calc (x + 1) (y + 2)] | y <- [0..(n-1)], x <- [0..(n-1)]]
          neighbors = map (filter (/= 0)) neighbors_0

rect2 n = [1..n*n]
rect n = zip (rect2 n) (rect0 n)

-- hold_neck
-- release_neck

-- perm l@(lx:lxs:lxss) neck@(nx:nxs:nxss) = (first, second)
    -- where nn = hold_neck
    --       first = map fst $ filter (\(a, b) -> b == 0) (zip lx nx)
    --       second = map fst $ filter (\(a, b) -> b == 0) (zip lxs nxs)
    --       comb = combinations first second

-- wrong_path = [0, (0, 0)]

neckit _ [] = []
neckit (i, e) ((xi, xe):xs) = (xi, t):(neckit (i, e) xs)
    where t1 = xe \\ [e]
          t2 = if xi == e then (t1 \\ [i]) else t1
          t = t2

perm ((i, es):xs) = map complete es
    where complete e = (neckit (i, e) xs)
        --   full = map complete es

perm_start l 0 = l
perm_start l n = perm_start dowork (n - 1)
    where dowork = (concat $ map perm l)

perm_begin l = perm_start [l] (length l)

func = length solutions
    where solutions = perm_begin (rect 4)

-- f(6) = 207408
-- func = rect 4

main = do
    print func
