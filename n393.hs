import Data.List

-- HELPER FUNCTION

chunksOf _ [] = []
chunksOf n l = (take n l):chunksOf n (drop n l)

-- function F splits each element of L into more elements then joins them
split f l = concat $ map f l

-- NEIGHBOR CREATION

rect0 n = neighbors
    where normal = chunksOf n [ x | x <- [1..(n * n)] ]
          zeros = (replicate (n + 3) 0) ++ (intercalate [0, 0] normal) ++ (replicate (n + 3) 0)
          calc x y = zeros !! ((n + 2) * y + x)
          neighbors_0 = [[calc (x + 1) (y), calc (x) (y + 1), calc (x + 2) (y + 1), calc (x + 1) (y + 2)] | y <- [0..(n-1)], x <- [0..(n-1)]]
          neighbors = map (filter (/= 0)) neighbors_0

rect2 n = [1..n*n]
rect n = zip (rect2 n) (rect0 n)

-- ACTUAL CODE

-- wrong_path l = any (\x -> length (ges x) == 0) l
--     where ges (_, es) = es

wrong_path [] = True
wrong_path l = (length $ ges $ last l) == 0
    where ges (_, es) = es

neckit _ [] = []
neckit _ ((_, []):_) = []
neckit (i, e) ((xi, xes):xs) = (xi, t):(neckit (i, e) xs)
    where t1 = xes \\ [e] -- transform 1: remove e
          t2 = if xi == e then (t1 \\ [i]) else t1 -- transform 2: remove i
          t = t2

-- [a] -> [[a]]
perm nb = (map (\e -> neckit (i, e) other) es)
    where first = head nb
          other = tail nb
          i = fst first
          es = snd first

each_l [] = []
each_l (x:xs)
    | wrong_path x = each_l xs
    | otherwise = perm x ++ each_l xs

begin l 0 = l
begin l n = begin (each_l l) (n - 1)

-- func n = begin [l] 4
func n = begin [l] (length l)
    where l = rect n

main = do
    print $ (length $ func 6)
