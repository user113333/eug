removeAt n xs =
    let get_el 0 (x:_) = x
        get_el n (_:xs) = get_el (n - 1) xs
        ignore_el _ [] = []
        ignore_el 0 (_:xs) = ignore_el (-1) xs
        ignore_el n (x:xs) = x:ignore_el (n-1) xs
     in (get_el n xs, ignore_el n xs)

main = do
    print $ removeAt 2 "abcd"
