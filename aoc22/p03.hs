import System.IO
import Data.List
import Data.List.Split
import qualified Data.Set as Set

-- original eval
-- eval c
--     | d >= fromEnum 'a' && d <= fromEnum 'z' = d - fromEnum 'a' + 1
--     | d >= fromEnum 'A' && d <= fromEnum 'Z' = d - fromEnum 'A' + 27
--         where d = fromEnum c

-- eval with maybe
eval c = case elemIndex c (['a'..'z'] ++ ['A'..'Z']) of
    Just a -> a + 1
    Nothing -> 0 -- should not happen the problem specifically says this is impossible

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
    print $ func_1 $ lines c
