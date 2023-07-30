pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) =
    let (a, b) = span (\y -> y == x) xs
     in [x:a] ++ pack b

encode :: Eq a => [a] -> [(Int, a)]
encode = (map (\xs -> (length xs, head xs))) . pack

data MaybeCount x y = Single y | Multiple x y
    deriving (Show)

to_mc :: (Int, a) -> MaybeCount Int a
to_mc (x, y) = if x == 1 then Single y else Multiple x y

encodeModified = (map to_mc) . encode
-- encodeModified = encode

main = do
    print $ encodeModified "aaaabccaadeeee"
