chain :: Integer -> [Integer]
chain i
    | i == 1 = [i]
    | otherwise = i:chain(n)
    where n | even i = (div i 2) | otherwise = (3 * i + 1)

chain_inv i
    | i > 1000000 = [i]
    | otherwise = i:chain_inv(n)
    where n | (even i || i == 1) = i * 2 | otherwise = (div (i - 1) 3)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

chain_repeat i
    | i > 100 = i
    | otherwise
    where n0 = i * 2
          n1 = div (n - 1) 3
          pass = mod (n - 1) 3

func = 

main = do
    print func
