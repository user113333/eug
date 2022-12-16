import System.IO

maximum' [] = -1
maximum' l = maximum l

takeWhile' f [] = []
takeWhile' f (x:xs)
    | f x = x:(takeWhile' f xs)
    | otherwise = x:[]

-- start with 0
top x y l = reverse $ map (!! x) $ take y l
bottom x y l = top x (length l - y - 1) $ reverse l
left x y l = reverse $ take x $ l !! y
right x y l = drop (x + 1) $ l !! y
get x y l = (l !! y) !! x
get_all x y l = [top x y l, right x y l, bottom x y l, left x y l]
can_see x y l = (maximum' (top x y l) < v) || (maximum' (bottom x y l) < v) || (maximum' (left x y l) < v) || (maximum' (right x y l) < v)
    where v = get x y l

ht x y l = product $ map (length . (takeWhile' (< get x y l))) (get_all x y l)

convert ls = map (map (\x -> fromEnum x - fromEnum '0')) ls

func ls = maximum [ ht x y l | x <- [0..ma], y <- [0..ma] ]
    where
        l = convert ls
        ma = length l - 1

main = do
    f <- openFile "data/p08.txt" ReadMode
    c <- hGetContents f
    print $ func $ lines c
