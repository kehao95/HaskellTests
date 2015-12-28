module Solution where
days :: Integer -> Integer -> Integer
days year month
    | month `elem` [1,3,5,7,8,10,12]    = 31
    | month `elem` [4,6,9,11]           = 30
    | month == 2  && leapyear           = 29
    | month == 2                        = 28        
    | otherwise                         = error "invalid month" 
    where leapyear = mod year 400 == 0 || (mod year 100 /= 0  &&  mod year 4 == 0)

solution = days
