import Data.List

hints = [ 319, 680, 180, 690, 129, 620, 762, 689, 762, 318, 368, 710, 720, 710, 629, 168, 160, 689, 716, 731, 736, 729, 316, 729, 729, 710, 769, 290, 719, 680, 318, 389, 162, 289, 162, 718, 729, 319, 790, 680, 890, 362, 319, 760, 316, 729, 380, 319, 728, 716 ]
chains = [ map (\x -> read [x] :: Int) chain | c <- hints, let chain = show c ]
chains_distinct = (nub . concat) chains

-- (element, count left, count right)
-- count_lr c n = (n, (nub . concat) l, (nub . concat) r)
count_lr c n = (n, (length . nub . concat) l, (length . nub . concat) r)
    where ciq = filter (elem n) c
          l = map (takeWhile (/=n)) ciq
          r = map (tail . (dropWhile (/=n))) ciq

count_lr_all c = map (count_lr c) chains_distinct

qs_triple [] = []
qs_triple a = l ++ [head a] ++ r
    where (_, h, _) = head a
          l = qs_triple $ filter (\(_, a0, _) -> a0 < h) (tail a)
          r = qs_triple $ filter (\(_, a0, _) -> a0 >= h) (tail a)

ordered = qs_triple $ count_lr_all chains

-- triple_first
tf (f, _, _) = f

func = foldl (\a b -> a ++ (show $ tf b)) "" ordered

main = do
    print func
