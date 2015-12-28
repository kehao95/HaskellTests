module Solution where
series :: Integer -> Integer
solution = series
series n = fibs !! ( fromInteger n) 
fibs  = 0:1:1:next fibs
	where 
		next (a:t@(b:c:_)) = (a+b+c):next t