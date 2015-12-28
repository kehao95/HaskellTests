module Solution where
polygon_area  :: [(Int, Int)] -> Rational
solution = polygon_area
polygon_area points =abs $ toRational ((x_diff_y points) - (y_diff_x points))/2



x_diff_y :: [(Int, Int)] -> Int
x_diff_y [(x,y)]  = 0
x_diff_y ((x0,y0):rest)  =  x0-snd (head rest) + x_diff_y rest

y_diff_x :: [(Int, Int)] -> Int
y_diff_x [(x,y)]  = 0
y_diff_x ((x0,y0):rest)  =  y0-fst (head rest) + x_diff_y rest