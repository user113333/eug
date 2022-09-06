replace [] _ _ = []
replace (x:xs) t e
    | x == t = e:other
    | otherwise = x:other
    where other = replace xs t e

left_side [] = []
left_side (x:xs) = head x:left_side xs

right_side [] = []
right_side (x:xs) = last x:right_side xs

p18 [] = 0
p18 (x:xs) = x !! 0 + p18 (map (if left then init else tail) xs)
    where left = sum (left_side xs) > sum (right_side xs)

func s = p18 [ [ read y :: Integer | y <- lines (replace x ' ' '\n') ] | x <- (lines s) ]

main :: IO ()
main = do
    s <- readFile "data/p18.txt"
    print (func s)
