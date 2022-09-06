import Data.List (sort, findIndex)

divisorsf n = concat ([[m, div n m] | m <-[1..floor(sqrt(fromIntegral n))], mod n m == 0])

triag_couple x = [x, 2*x+1]
--1 : concat (map triag_couple [1..10])
 
multx (x:xs) 
  | xs == [] = []
  | otherwise = x * (head xs) : multx(xs)
  
--findIndex (>= 500) multx(map (length . divisorsf) (1 : concat (map triag_couple [1..])))

main = do
--   print(1 : concat (map triag_couple [1..10]))
--   print(multx (1 : concat (map triag_couple [1..10])))
  --print(triag_couple 10)
  --print(map (sort.divisorsf) (triag_couple 10))
  --print((foldl (*) 1 (map (length . divisorsf) (triag_couple 10))))
  --print(sort $ divisorsf 210) 
  --print(length $ divisorsf 210)
  print(findIndex (>= 500) (multx (map (length . divisorsf) (1 : concat (map triag_couple [1..])))))
--   print((1 : concat (map triag_couple [1..]))!!12374 * (1 : concat (map triag_couple [1..]))!!12375)

