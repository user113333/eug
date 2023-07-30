import Data.List

compress l = map head $ group l

main = do
    print $ compress "aaaabccaadeeee"
