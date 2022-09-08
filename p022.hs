import Data.List

remove_all e [] = []
remove_all e (x:xs)
    | x == e = remove_all e xs
    | otherwise = x:remove_all e xs

split_all _ [] = []
split_all e l = x:split_all e (drop (length x + 1) l)
    where x = takeWhile (e /=) l

qss [] = []
qss l = f ++ [h] ++ e
    where h = head l
          f = qss $ filter (h>) (tail l)
          e = qss $ filter (h<=) (tail l)

-- alphabetical value
av [] = 0
av (x:xs) = (fromEnum x) - (fromEnum '@') + (av xs)

-- func s = nub $ foldl1 (++) t
func s = sum [ i * (av (t !! (i - 1))) | i <- [1..length t] ]
    where t0 = remove_all '"' s
          t1 = remove_all '\n' t0
          t2 = split_all ',' t1
          t3 = qss t2
          t = t3

-- 871198282
main = do
    s <- readFile "data/p022.txt"
    print $ func s
