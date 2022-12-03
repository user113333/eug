import System.IO
import Data.List.Split
import qualified Data.Set as Set

eval c
    | d >= fromEnum 'a' && d <= fromEnum 'z' = d - fromEnum 'a' + 1
    | d >= fromEnum 'A' && d <= fromEnum 'Z' = d - fromEnum 'A' + 27
        where d = fromEnum c

el_1 l = eval $ head $ Set.toList $ Set.intersection (Set.fromList a) (Set.fromList b)
    where (a:b:[]) = chunksOf (div (length l) 2) l

el_2 (la:lb:lc:[]) = eval $ head $ Set.toList $ a `Set.intersection` b `Set.intersection` c
    where
        a = Set.fromList la
        b = Set.fromList lb
        c = Set.fromList lc

func_1 ls = sum $ map el_1 ls

func_2 ls = sum $ map el_2 (chunksOf 3 ls)

main = do
    f <- openFile "data/p03.txt" ReadMode
    c <- hGetContents f
    print $ func_2 $ lines c
