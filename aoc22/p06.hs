import System.IO
import Data.List

subll l1 l2 = length l1 - length l2

ssplit [] _ = []
ssplit all@(_:xs) c
    | (length $ nub $ take c all) == c = drop c all
    | otherwise = ssplit xs c

func ls c = subll l (ssplit l c)
    where l = head ls

func_1 ls = func ls 4
func_2 ls = func ls 14

main = do
    f <- openFile "data/p06.txt" ReadMode
    c <- hGetContents f
    print $ func_2 $ lines c
