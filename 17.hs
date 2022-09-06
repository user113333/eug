-- zero one two three four five six seven eight nine
ones = [ 0, 3, 3, 5, 4, 4, 3, 5, 5, 4 ]
-- 0 one(for hundreds) twenty thirty fourty fifty sixty seventy eighty ninety
tens = [ 0, 3, 6, 6, 6, 5, 5, 7, 6, 6 ]
-- 
hundreds = map (+7) ones

always3 n = replicate (3 - length n) 0 ++ n
count_char n = one (read (head s)) + ten (read (s !! 1)) + hundred (read (last s))
    where s = always3 (show n)
          one n = ones !! n
          ten n = if n == 1 then tens_one n

main = do
    print func
