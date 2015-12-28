module Solution where
merge_sort  :: [Int]->[Int]->[Int]
solution = merge_sort
merge_sort xs [] = xs 
merge_sort [] ys = ys
merge_sort (x:xs) (y:ys)
	| x<y = x:merge_sort xs (y:ys)
	| x>y = y:merge_sort (x:xs) ys
	|otherwise = x:y:merge_sort xs ys
