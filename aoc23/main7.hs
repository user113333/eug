import Data.List

-- AOC23 day 4 part 1

parse_line x = if ic > 0 then 2 ^ (ic - 1) else 0
    where
        (left, right) = break (== "|") $ drop 2 x
        game = left
        my = drop 1 right
        ic = length $ intersect game my

parse x = sum $ map (parse_line . words) x

main = do
    x <- getContents
    print $ parse $ lines x
