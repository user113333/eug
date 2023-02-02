import System.IO

triangle i = div (i * (i + 1)) 2

isTriangle n = last (takeWhile (\x -> x <= n) tl) == n
    where tl = map triangle [0..]

evalWord a = sum $ map (\x -> fromEnum x - fromEnum 'A' + 1) a

func l = length $ filter isTriangle a
    where a = map evalWord l

main = do
    h <- openFile "data/p042.txt" ReadMode
    s <- hGetContents h
    print $ func $ lines $ s
