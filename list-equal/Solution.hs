module Solution where
equal :: [Integer] -> [Integer] -> Bool
equal [] []     = True
equal (x:_) [] = False
equal [] (x:_) = False
equal (x:xs) (y:ys)
    | x == y    = True && equal xs ys
    | otherwise = False

solution = equal
