isqrt :: Int -> Int
isqrt = round . sqrt . fromIntegral

is_target :: Float -> Float -> Bool
is_target a b = sqrt(a^2 + b^2) + a + b == 1000

ab = [ (a, b) | a <- [1..1000], b <- [1..1000], (is_target (fromIntegral (a) :: Float) (fromIntegral (b) :: Float)) ]

abc (a, b) = [ a, b, isqrt(a^2 + b^2) ]

-- func = ab
func = product (abc (head ab))

main = do
    print func
