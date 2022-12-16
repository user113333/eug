import System.IO

maximum' [] = -1
maximum' l = maximum l

-- start with 0
top x y l = map (!! x) $ take y l
bottom x y l = reverse $ top x (length l - y - 1) $ reverse l
left x y l = take x $ l !! y
right x y l = drop (x + 1) $ l !! y
get x y l = (l !! y) !! x
can_see x y l = (maximum' (top x y l) < v) || (maximum' (bottom x y l) < v) || (maximum' (left x y l) < v) || (maximum' (right x y l) < v)
    where v = get x y l

convert ls = map (map (\x -> fromEnum x - fromEnum '0')) ls

func ls = foldl (\a b -> if b then a + 1 else a + 0) 0 [ can_see x y l | x <- [0..ma], y <- [0..ma] ]
-- func ls = [ (x, y, can_see x y l) | x <- [0..ma], y <- [0..ma] ]
    where
        l = convert ls
        ma = length l - 1

main = do
    f <- openFile "data/p08.txt" ReadMode
    c <- hGetContents f
    print $ func $ lines c
