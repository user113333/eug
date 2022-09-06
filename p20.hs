factorial 1 = 1
factorial n = n * factorial (n - 1)

sum_char [] = 0
sum_char (x:xs) = read [x] + sum_char xs

func = sum_char $ show $ factorial 100

main = do
    print func
