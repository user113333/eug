import Data.List

-- AOC23 day 4 part 2

zipWithG f [] [] = []
zipWithG f (a:as) [] = a : (zipWithG f as [])
zipWithG f [] (a:as) = a : (zipWithG f [] as)
zipWithG f (a:as) (b:bs) = (f a b) : (zipWithG f as bs)

wins line = length $ intersect left right
    where
        wrds = tail $ tail $ words line
        left = map read (fst $ break (== "|") wrds) :: [Int]
        right = map read (tail $ snd $ break (== "|") wrds) :: [Int]

new_scratches line n_of_current = replicate (wins line) n_of_current

-- lines, n of scratches
rec [] _ = []
rec (l:ls) (w:ws) = (w + 1) : (rec ls $
    zipWithG (+) adds (if null ws then [0] else ws))
    where
        adds = new_scratches l (w + 1)

parse lines = sum $ rec lines [0]

main = do
    x <- getContents
    print $ parse $ lines x
