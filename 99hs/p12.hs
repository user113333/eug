data MaybeCount x y = Single y | Multiple x y
    deriving (Show)

from_mc :: MaybeCount Int a -> [a]
from_mc (Single y) = [y]
from_mc (Multiple x y) = replicate x y

decodeModified = (foldl1 (++)) . (map from_mc)

main = do
    print $ decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
