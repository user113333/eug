split xs n =
    let take' 0 _ = []
        take' n (x:xs) = x:(take' (n - 1) xs)
        drop' 0 xs = xs
        drop' n (x:xs) = drop' (n - 1) xs
     in (take' n xs, drop' n xs)

main = do
    print $ split "abcdefghik" 3
