module AlphabeticAnagrams where
import Data.List

-- Drone, Overlord, Drone, 3x Drone, Hatch

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = product [1..n]

getLess c l = filter (\x -> x < c) l

calcPerms :: (Ord a) => [a] -> Integer
calcPerms l = div (factorial $ toInteger $ length l) (product (map (\x -> factorial $ toInteger $ length x) (group $ sort l)))

lexiPos' :: String -> Integer
lexiPos' [] = 1
lexiPos' (x:xs) = calc + lexiPos' xs
    where
        calc = sum $ map (\a -> calcPerms (x:(delete a xs))) (nub $ getLess x xs)

lexiPos :: String -> Integer
lexiPos s = lexiPos' s
