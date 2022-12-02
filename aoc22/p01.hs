import System.IO
import Data.List
import Control.Monad

sp l = span (\x -> length x /= 0) l

degroup [] = []
degroup l = f:(degroup $ tail rest)
    where (f, rest) = sp l

func_1 l = maximum $ map (foldl (\acc a -> (read a) + acc) 0) g
    where g = degroup (l ++ [""])

func_2 l = sum $ take 3 $ sortBy (\a b -> compare b a) $ map (foldl (\acc a -> (read a) + acc) 0) g
    where g = degroup (l ++ [""])

main = do
    f <- openFile "data/p01.txt" ReadMode
    c <- hGetContents f
    print $ func_2 $ lines c