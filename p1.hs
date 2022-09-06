-- solution 1
func = sum [ x | x <- [0..999], (mod x 5) == 0 || (mod x 3) == 0]

-- solution 2
elm c n = (((c/n) + 1) / 2) * c
func2 = (elm 999 3) + (elm 995 5) - (elm 990 15)

main = do
    print func2
