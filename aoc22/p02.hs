import System.IO
-- import Data.Char (ord)

win = 6
draw = 3
lose = 0

result_1 (1, 1) = draw
result_1 (1, 2) = win
result_1 (1, 3) = lose
result_1 (2, 1) = lose
result_1 (2, 2) = draw
result_1 (2, 3) = win
result_1 (3, 1) = win
result_1 (3, 2) = lose
result_1 (3, 3) = draw

result_2 (1, 1) = 3
result_2 (1, 2) = 1
result_2 (1, 3) = 2
result_2 (2, 1) = 1
result_2 (2, 2) = 2
result_2 (2, 3) = 3
result_2 (3, 1) = 2
result_2 (3, 2) = 3
result_2 (3, 3) = 1

outcome 1 = 0
outcome 2 = 3
outcome 3 = 6

-- simply_glyph s = (fromEnum 'A') + 1
simply_glyph s = (convert $ head s, convert $ transform $ last s)
    where
        convert c = fromEnum c + 1 - fromEnum 'A'
        transform 'X' = 'A'
        transform 'Y' = 'B'
        transform 'Z' = 'C'

score_1 r = result_1 pair + snd pair
    where
        pair = simply_glyph r

score_2 r = (result_2 pair) + (outcome $ snd pair)
    where
        pair = simply_glyph r

func_1 l = sum $ map score_1 l
func_2 l = sum $ map score_2 l

main = do
    f <- openFile "data/p02.txt" ReadMode
    c <- hGetContents f
    print $ func_2 $ lines c