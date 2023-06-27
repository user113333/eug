-- https://www.codewars.com/kata/534e01fbbb17187c7e0000c6/train/haskell
-- this code fucking sucks

module Spiral where

overpaint s (vx, vy, vv) = zipWith (\line y -> zipWith (\ov x -> if (x == vx) && (vy == y) then vv else ov) line [0..]) s [0..]
overpaint_list s [] = s
overpaint_list s (x:xs) = overpaint_list (overpaint s x) xs
create_spiral n = [ [ 0 | y <- [1..n] ] | x <- [1..n] ]

squares' s lvl n = if (lvl >= (div (n + 1) 2)) then [] else sq ++ squares' s (lvl + 2) n
    where
        sq = lx1 ++ ly1 ++ lx2 ++ ly2
        lx1 = [(x, lvl, 1) | x <- [lvl..(n-lvl-1)] ]
        ly1 = [(lvl, x, 1) | x <- [lvl..(n-lvl-1)] ]
        lx2 = [(x, n-1-lvl, 1) | x <- [lvl..(n-lvl-1)] ]
        ly2 = [(n-1-lvl, x, 1) | x <- [lvl..(n-lvl-1)] ]

squares s lvl n = (squares' s lvl n) ++ if (odd n) && ((mod n 4) == 0) then [(div n 2, div n 2, 1)] else []

inverts n = [ (x, 1 + x, if even x then 0 else 1) | x <- [0..(div (n-3) 2)] ]

spiralize' s n = overpaint_list s points_list
    where
        points_list = (squares s 0 n) ++ (inverts n) ++ spcase
        spcase = if even n then [((div n 2) - 1, div n 2, 0)] else []

spiralize :: Int -> [[Int]]
spiralize n = spiralize' (create_spiral n) n
