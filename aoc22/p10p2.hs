import System.IO
import Data.List
import Data.List.Split

get_pixel x = if x >= 0 && x <= 2 then "█" else " "

mul_pair (a, b) = a * b

tail' [] = []
tail' l = tail l
span' f l = let (a, b) = span f l in (a, tail' b)

pc (cmd, val) (cyc, ses)
    | cmd == "noop" = (cyc + 1, ses)
    | otherwise = (cyc + 2, ses + read val)
    where x = mod cyc 40

pl l = scanl (\a b -> b a) (0, 0) (map (\x -> pc (span' (/=' ') x)) l)

start 240 _ = []
start cyc l = (get_pixel (x-s)):start (cyc + 1) l
    where
        s = snd $ last (takeWhile (\x -> fst x <= cyc) l)
        x = mod cyc 40


func ls = unlines $ chunksOf 40 $ concat $ start 0 res
    where res = pl ls

main = do
    f <- openFile "data/p10.txt" ReadMode
    c <- hGetContents f
    putStrLn $ func $ lines c
