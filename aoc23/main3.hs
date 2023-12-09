-- AOC23 day 2 part 1

drop_second [] = []
drop_second (x:[]) = [x]
drop_second (x:_:xs) = x:drop_second xs
drop_chars _ [] = []
drop_chars chrs (x:xs) = if not $ x `elem` chrs then x:(drop_chars chrs xs) else drop_chars chrs xs

test_hand num color
    | color == "red" = num <= 12
    | color == "green" = num <= 13
    | color == "blue" = num <= 14
parse_line l = if res then game else 0
    where
        line = map (drop_chars [':', ';', ',']) $ words l
        line_hands = drop 2 line
        game = read $ line !! 1 :: Int
        hands = zip (drop_second line_hands) (drop_second $ drop 1 line_hands)
        hands_tested = map (\(a, b) -> test_hand (read a) b) hands
        res = all id hands_tested

parse l = sum $ map parse_line $ lines l

main = do
    inp <- getContents
    print $ parse inp
