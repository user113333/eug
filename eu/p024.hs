import Data.List

options = ['0'..'9']
solutions = [2, 6, 6, 2, 5, 1, 2, 1, 1]

start l [] = l
start l (x:xs) = [ni] ++ (start (delete ni l) xs)
    where nl = sort l
          ni = nl !! x

func = start options solutions

main = do
    print func
