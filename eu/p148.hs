pt_nrow r = zipWith (+) (r ++ [0]) (0:r)
pt_recursive 0 _ = []
pt_recursive n r = nr:pt_recursive (n - 1) nr
    where nr = pt_nrow r
pt n = pt_recursive n [1]

func = pt 12

-- NOT_DONE
main = do
    print func
