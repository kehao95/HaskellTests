module Solution where
cross_one :: Int -> [a] -> [a] -> ([a], [a])
solution = cross_one
cross_one n list_a list_b = (((take n list_b)++(drop n list_a)),((take n list_a)++(drop n list_b)))