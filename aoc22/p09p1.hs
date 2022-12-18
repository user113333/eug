import System.IO
import Data.List

span' f l = let (a, b) = span f l in (a, tail b)

inv_cmd "U" = "D"
inv_cmd "D" = "U"
inv_cmd "L" = "R"
inv_cmd "R" = "L"

point_empty = ((0, 0), (0, 0))
touching ((a, b), (c, d)) = not ((abs (a - c) > 1) || (abs (d - b) > 1))
tail_move cmd ((na, nb), (c, d)) = if touching ((na, nb), (c, d)) then (c, d) else point_move (inv_cmd cmd) (na, nb)
point_move cmd (a, b)
    | cmd == "U" = (a, b + 1)
    | cmd == "D" = (a, b - 1)
    | cmd == "L" = (a - 1, b)
    | cmd == "R" = (a + 1, b)

list_move cmd t = (pm, tm):t
    where
        ((a, b), (c, d)) = head t
        pm = point_move cmd (a, b)
        tm = tail_move cmd (pm, (c, d))

pc c r t = last $ take (r + 1) $ iterate pm t
    where pm = list_move c

pl [] t = t
pl (x:xs) t = pl xs $ pc a (read b :: Int) t
    where (a, b) = span' (' '/=) x

func ls = length $ nub $ map snd sim
    where sim = pl ls [point_empty]

main = do
    f <- openFile "data/p09.txt" ReadMode
    c <- hGetContents f
    print $ func $ lines c
