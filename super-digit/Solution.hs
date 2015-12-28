module Solution where
super_digit :: Integer -> Integer -> Integer
super_digit n k
    | k > 1 = super_digit ((super_digit n 1)*k) 1
    | n < 10    =  n
    | otherwise = super_digit (foo n) 1
    where
        foo ::Integer -> Integer 
        foo n =   if n< 10 then n else mod n 10 + foo (div n 10)

solution = super_digit
