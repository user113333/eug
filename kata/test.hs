module Test where

test val expected = if val == expected then putStrLn "PASS" else putStrLn ("FAIL - expected: " ++ (show expected) ++ " got: " ++ (show val))
