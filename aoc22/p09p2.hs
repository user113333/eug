import System.IO
import Data.List

span' f l = let (a, b) = span f l in (a, tail b)

is_point_positive (a, b) = a >= 0 && b >= 0
point_empty = ((0, 0), replicate 9 (0, 0))
touching ((a, b), (c, d)) = not ((abs (a - c) > 1) || (abs (d - b) > 1))
tail_move cmd ((na, nb), (c, d)) = if touching ((na, nb), (c, d)) then (c, d) else point_move_diag (na, nb) (c, d)
point_move cmd (a, b)
    | cmd == "U" = (a, b + 1)
    | cmd == "D" = (a, b - 1)
    | cmd == "L" = (a - 1, b)
    | cmd == "R" = (a + 1, b)
point_move_diag (a, b) (c, d) = (c + signum (a - c), d + signum (b - d))

all_tail_move cmd h [] = []
all_tail_move cmd h (x:xs) = tm:(all_tail_move cmd tm xs)
    where tm = tail_move cmd (h, x)

list_move cmd t = (pm, tm):t
    where
        ((a, b), tails) = head t
        pm = point_move cmd (a, b)
        tm = all_tail_move cmd pm tails

pc c r t = last $ take (r + 1) $ iterate pm t
    where pm = list_move c

pl [] t = t
pl (x:xs) t = pl xs $ pc a (read b :: Int) t
    where (a, b) = span' (' '/=) x

func ls = length $ nub $ map last $ map snd sim
    where sim = pl ls [point_empty]

main = do
    f <- openFile "data/p09.txt" ReadMode
    c <- hGetContents f
    print $ func $ lines c
