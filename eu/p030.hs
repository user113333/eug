stevke x = if x < 10 then [x] else rem x 10:(stevke $ quot x 10)
pow_stevke x = sum $ map (\x -> x^5) (stevke x)

func = sum [ x | x <- [10..531441], x == pow_stevke x ]

main = print func
