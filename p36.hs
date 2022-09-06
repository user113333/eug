to_binary 1 = "1"
to_binary n = to_binary (div n 2) ++ (show . mod n) 2

is_palindrome s = s == reverse s

both_palindrome n = is_palindrome decimal && is_palindrome binary
    where
        decimal = show n
        binary = to_binary n

func = sum [ n | n <- [1..1000000], both_palindrome n ]

main = do
    print func
