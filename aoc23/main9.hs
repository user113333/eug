-- AOC23 day 5 part 1

inrange src (_, b, c) = src >= b && src < (b + c)
list_to_triple (a:b:c:[]) = (a, b, c)
split list target = left : other
    where
        (left, right) = break (== target) list
        other = if null right then [] else split (tail right) target

parse_ranges :: String -> (Integer, Integer, Integer)
parse_ranges line = list_to_triple $ map read $ words line

to_seeds :: [String] -> [Integer]
to_seeds (line:_) = map read $ tail $ words line

to_chunks :: [String] -> [[(Integer, Integer, Integer)]]
to_chunks lines = (map . map) parse_ranges $ map tail $ split (tail $ tail lines) ""

-- source, chunk
coresponding src [] = src
coresponding src ((a, b, c):ms)
    | inrange src (a, b, c) = a + (src - b)
    | otherwise = coresponding src ms

parse_sources sources chunk = map (\a -> coresponding a chunk) sources

func lines = minimum $ foldl (\l r -> parse_sources l r) seeds chunks
    where
        seeds = to_seeds lines
        chunks = to_chunks lines

main = do
    c <- getContents
    print $ func $ lines c
