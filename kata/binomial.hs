import Text.Regex.TDFA
import System.Exit
import Test

var :: String -> String
var s = s =~ "[a-z]"

val_a :: (Read a, Num a) => String -> a
val_a s = if null r then 1 else (read (take (length r - 1) r))
    where r = s =~ "-?[0-9]+[a-z]" :: String

val_b :: (Read a, Num a) => String -> a
val_b s = read (r =~ "-?[0-9]+")
    where r = s =~ "[a-z][\\+-][0-9]+" :: String

-- Exponent of the expresion
rexp :: (Read a, Num a) => String -> a
-- rexp :: String -> String
rexp s = read $ drop 1 r
    where r = s =~ "\\^[0-9]+" :: String

factorial 0 = 1
factorial n = product [1..n]

binomial a b = div (factorial a) (factorial b * factorial (a - b))

expand'' a b e = map expansion [0..e]
    where expansion i = (a^(e-i) * b^i) * binomial e i

expand' s = joinwithplus $ zipWith expansion (expand'' a b e) [0..]
    where
        k = var s
        a = val_a s
        b = val_b s
        e = rexp s
        lastk i n = case i of
            0 -> ""
            1 -> k
            _ -> k ++ "^" ++ (show i)
        expansion n i = (if (n == 1) && (e /= i) then "" else show n) ++ lastk (e - i) n
        joinwithplus l = foldl1 (\a b -> if null b then a else (if b !! 0 == '-' then a ++ b else a ++ "+" ++ b)) l

expand s = if rexp s == 0 then "1" else expand' s

main = do
    test (expand "(x-1)^0") "1"
    test (expand "(x+1)^0") "1"
    test (expand "(x+1)^1") "x+1"
    test (expand "(x+1)^2") "x^2+2x+1"
    test (expand "(x-1)^0") "1"
    test (expand "(x-1)^1") "x-1"
    test (expand "(x-1)^2") "x^2-2x+1"
    test (expand "(5m+3)^4") "625m^4+1500m^3+1350m^2+540m+81"
    test (expand "(2x-3)^3") "8x^3-36x^2+54x-27"
    test (expand "(7x-7)^0") "1"
    test (expand "(-5m+3)^4") "625m^4-1500m^3+1350m^2-540m+81"
    test (expand "(-2k-3)^3") "-8k^3-36k^2-54k-27"
    test (expand "(-7x-7)^0") "1"

