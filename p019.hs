md = [ 31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
month_days m y
    | m == 1 = if mod y 4 == 0 && mod y 400 /= 0 then 29 else 28
    | otherwise = md !! m

-- (days since 1.1.1901, m, y)
-- month starts with 0
next_month (d, m, y) = (d + month_days m y, nm, ny)
    where 
          ny = if m >= 11 then y + 1 else y
          nm = mod (m+1) 12

start (d, m, y)
    | y > 2000 = 0
    | otherwise = (if mod d 7 == 0 then (1 +) else (0 +)) (start $ next_month (d, m, y))

-- 1.1.1901 = SRE
func = start (3, 1, 1901)

main = print func
