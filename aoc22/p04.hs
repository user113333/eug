import System.IO
import qualified Data.Set as S

isTrue x = x

split c l = (a, tail b)
    where (a, b) = splitAt (length $ takeWhile (/= c) l) l

row r = ((read a1 :: Int, read a2 :: Int), (read b1 :: Int, read b2 :: Int))
    where
        (a, b) = split ',' r
        (a1, a2) = split '-' a
        (b1, b2) = split '-' b

eval_1 r = null (a S.\\ b) || null (b S.\\ a)
    where
        ((a1, a2), (b1, b2)) = row r
        a = S.fromList [a1..a2]
        b = S.fromList [b1..b2]

eval_2 r = not $ null $ S.intersection a b
    where
        ((a1, a2), (b1, b2)) = row r
        a = S.fromList [a1..a2]
        b = S.fromList [b1..b2]

func_1 ls = length $ filter isTrue $ map eval_1 ls

func_2 ls = length $ filter isTrue $ map eval_2 ls

main = do
    f <- openFile "data/p04.txt" ReadMode
    c <- hGetContents f
    print $ func_2 $ lines c
