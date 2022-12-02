replace [] _ _ = []
replace (x:xs) t e
    | x == t = e:other
    | otherwise = x:other
    where other = replace xs t e

matrix2tree m = [ [ l !! i | i <- [0..(length l - 1)], indices !! i == tree_i ] | tree_i <- [0..(2 * (s - 1))]]
-- matrix2tree m = [indices, l]
    where indices = [ x + y | x <- [0..s - 1], y <- [0..s - 1] ]
          l = concat m
          s = length (head m)

connect top bottom = [ x + min p0 p1 | (x, (p0, p1)) <- (zip bottom pairs) ]
    where
        reversed = length top > length bottom
        top0 = if reversed then init top else [999999] ++ top
        top1 = if reversed then tail top else  top ++ [999999]
        pairs = zip top0 top1

p81 (x:[]) = x
p81 (x:y:xs) = p81 (connect top bottom:xs)
    where top = x
          bottom = y

maxm m = [ maximum x | x <- m ]

-- 9.5s
func s = out
    where matrix_s = map (\x -> lines (replace x ',' '\n')) (lines s)
          matrix = map (map (\x -> read x :: Int)) matrix_s
          tree = matrix2tree matrix
          out = head $ p81 tree

-- 320706
-- 427337
main :: IO ()
main = do
    s <- readFile "data/p081.txt"
    print (func s)
