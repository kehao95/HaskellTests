module Solution where
alpha_list = " zyxwvutsrqponmlkjihgfedcba"
foo :: Int -> Int -> [[Char]]
foo 0 b = [[]]
foo 1 b = [[x] | x <- [alpha_list!!b..'z']]
foo f b = [(alpha_list!!x):y | x <- [b, b-1..f], y <- foo (f-1) (x-1)]

solution :: Integer -> [[Char]]
solution n = foo (fromEnum n) 26