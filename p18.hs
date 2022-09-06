replace [] _ _ = []
replace (x:xs) t e
    | x == t = e:other
    | otherwise = x:other
    where other = replace xs t e

connect top bottom = [ x + max p0 p1 | (x, (p0, p1)) <- (zip bottom pairs) ]
    where
        top0 = [0] ++ top
        top1 = top ++ [0]
        pairs = zip top0 top1

p18 (x:[]) = x
p18 (x:y:xs) = p18 (connect top bottom:xs)
    where top = x
          bottom = y

func s = maximum (p18 tree)
    where tree = [ [ read y :: Integer | y <- lines (replace x ' ' '\n') ] | x <- (lines s) ];

main :: IO ()
main = do
    s <- readFile "data/p18.txt"
    print (func s)
