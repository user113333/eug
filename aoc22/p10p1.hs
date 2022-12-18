import System.IO
import Data.List

mul_pair (a, b) = a * b

tail' [] = []
tail' l = tail l
span' f l = let (a, b) = span f l in (a, tail' b)

pc (cmd, val) (cyc, ses)
    | cmd == "noop" = (cyc + 1, ses)
    | otherwise = (cyc + 2, ses + read val)

pl l = scanl (\a b -> b a) (0, 1) (map (\x -> pc (span' (/=' ') x)) l)

step_20 s l
    | s > 220 = []
    | otherwise = (s, b):(step_20 (s + 40) l)
    where (a, b) = (last $ takeWhile (\x -> s > fst x) l)

func ls = sum $ map mul_pair (step_20 20 res)
    where res = pl ls

main = do
    f <- openFile "data/p10.txt" ReadMode
    c <- hGetContents f
    print $ func $ lines c
