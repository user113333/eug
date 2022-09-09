import Data.List

func = length $ nub [ a^b | a <- [2..100], b <- [2..100] ]

main = do
    print func
