import System.IO
import Data.List
import Data.List.Split

remove a l = filter (/= a) l

ltr2num = map (\x -> read x :: Int)

tr_a a = map (\x -> x !! 1) (chunksOf 4 a)

tr ls = (a0, b0)
    where
        a0 = map (remove ' ') (transpose $ map tr_a (init a))
        b0 = map (\x -> ltr2num $ (splitOn " " x) \\ ["move", "from", "to"]) b
        (a, _:b) = splitAt (head $ elemIndices "" ls) ls

begin a [] _ = a
begin a ((x:y:z:[]):xs) use_r = begin ta xs use_r
    where
        chunk_br = take x (a !! (y - 1))
        chunk = if use_r then reverse chunk_br else chunk_br
        ta = [ le (a !! i) (i + 1) | i <- [0..(length a - 1)]]
        le e i | i == y = drop x e
               | i == z = chunk ++ e
               | otherwise = e

func ls use_r = map head $ begin a b use_r
    where (a, b) = tr ls

func_1 ls = func ls True
func_2 ls = func ls False

main = do
    f <- openFile "data/p05.txt" ReadMode
    c <- hGetContents f
    print $ func_2 $ lines c
