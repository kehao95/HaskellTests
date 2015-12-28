module Solution where
couples :: [Integer] -> [Integer] -> Integer
couples [] [] = 0
couples (x:xs) [] = 1 + couples xs []
couples [] (x:xs) = 1 + couples xs []
couples (x:xs) (y:ys) 
    | x<y   =1+ couples xs (y:ys)
    | x==y  =couples xs ys
    | x>y   =1+ couples (x:xs) ys

solution = couples


