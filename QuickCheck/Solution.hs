module Solution where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
merge_sort  :: [Int]->[Int]->[Int]
merge_sort xs [] = xs 
merge_sort [] ys = ys
merge_sort (x:xs) (y:ys)
	| x<y = x:merge_sort xs (y:ys)
	| x>y = y:merge_sort (x:xs) ys
	|otherwise = x:y:merge_sort xs ys


quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

prop_value1 = (merge_sort [] [] == [])
prop_value2 = (merge_sort [1] [] == [1])


-- commutative
prop_commutative x y = (merge_sort xs ys == merge_sort ys xs)
						where 
							xs = sort x
							ys = sort y
-- smallest
prop_smallest x y = (not $ or [null x,null y])==>head sorted == minimum [minimum x,minimum y]
						where 
							xs = sort x
							ys = sort y
							sorted = merge_sort xs ys
-- largest
prop_largest x y = (not $ or [null x,null y])==>last sorted == maximum [maximum x,maximum y]
						where 
							xs = sort x
							ys = sort y
							sorted = merge_sort xs ys 
-- sum
prop_sum x y = sum x + sum y  == sum sorted 
						where 
							xs = sort x
							ys = sort y
							sorted = merge_sort xs ys 
-- count
prop_length x y = length x + length y  == length sorted
						where 
							xs = sort x
							ys = sort y
							sorted = merge_sort xs ys 
-- sort
prop_sort x y = sorted == sort (x ++ y)
						where 
							xs = sort x
							ys = sort y
							sorted = merge_sort xs ys 

