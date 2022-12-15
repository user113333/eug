import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as M

is_bash_start x = head x == '$'

dir_size k dict = size + dirs_size
    where
        (dirs0, sizes) = partition (\x -> take 3 x == "dir") (fromJust $ M.lookup k dict)
        dirs1 = map (\x -> k ++ (drop 4 x) ++ "/") dirs0
        dirs_size = sum $ map (\x -> dir_size x dict) dirs1
        size = sum (map (\x -> read $ fst $ span (/= ' ') x) sizes)

process_dirs dirs dict = map (\(dir, _) -> dir_size dir dict) dirs

-- returns (pwd, dict)
process_command (cmd, res) pwd dict
    | cmd == "$ cd /" = ("/", dict)
    | cmd == "$ cd .." = (reverse $ snd $ span (/= '/') $ reverse $ init $ pwd, dict)
    | cmd == "$ ls" = (pwd, M.insert pwd res dict)
    | otherwise = (pwd ++ (drop 5 cmd) ++ "/", dict)

-- returns [(cmd, res)]
commands [] = []
commands (x:xs) = (x, a):(commands b)
    where
        a = takeWhile (not . is_bash_start) xs
        b = dropWhile (not . is_bash_start) xs

func ls = minimum $ filter ((30000000 - (70000000 - dir_size "/" dict))<) sizes
    where
        (_, dict) = foldl (\(pwd, dict) f -> f pwd dict) ("/", M.empty) (map process_command (commands ls))
        sizes = process_dirs (M.toList dict) dict

main = do
    f <- openFile "data/p07.txt" ReadMode
    c <- hGetContents f
    print $ func $ lines c
