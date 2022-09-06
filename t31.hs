coin_sum n l@(x:xs) 
  | n == 0 = 1
  | n < 0 = 0
  | l == [2,1] = fst(divMod n 2) + 1
  | otherwise = coin_sum n xs + coin_sum (n - x) l

---------------------------------------------------
os = map (\x -> [x,2*x..1000]) [5,10,20,50,100,200]
sums12 = map (\x -> fst(divMod x 2) + 1) [0..1000]
offset l x = (take x $ repeat 0) ++ l 

generate_sums (l, o)
  | o == [] = (l,o)
  | otherwise = (l_new, tail(o))
  where
  o_list = map (offset l) (head o)
  l_new = foldl (\x y -> zipWith (+) x y) l o_list
  
  
main = do
  print (coin_sum 1000 [200,100,50,20,10,5,2,1]) 
--   print (fst(last (take (length os + 1) $ iterate generate_sums (sums12, os)))!!1000)
  
  