drop_second [] = []
drop_second (x:[]) = [x]
drop_second (x:_:xs) = x:drop_second xs
drop_chars _ [] = []
drop_chars chrs (x:xs) = if not $ x `elem` chrs then x:(drop_chars chrs xs) else drop_chars chrs xs
product_triple (a, b, c) = a * b * c

update_best num color (r, g, b)
    | color == "red" && num > r = (num, g, b)
    | color == "green" && num > g = (r, num, b)
    | color == "blue" && num > b = (r, g, num)
    | otherwise = (r, g, b)
parse_line l = product_triple best
    where
        line = map (drop_chars [':', ';', ',']) $ words l
        line_hands = drop 2 line
        game = read $ line !! 1 :: Int
        hands = zip (drop_second line_hands) (drop_second $ drop 1 line_hands)
        best = foldl (\f (a, b) -> update_best (read a) b f) (0, 0, 0) hands
        res = best

parse l = sum $ map parse_line $ lines l

main = do
    inp <- getContents
    print $ parse inp
