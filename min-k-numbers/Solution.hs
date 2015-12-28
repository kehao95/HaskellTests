module Solution where
import Data.List
solution::[Integer]->Int->[Integer]

solution _ 0 = []
solution xs n
    | len == 0  = (s:(solution (tail xs) (n-1)))
    | len > n   = solution left n
    | len <= n  = sort(left) ++ solution right (n - len)
    where   s = xs!!0
            left = [x|x<-xs,x < s]
            right = [x|x<-xs, x >= s]
            len = length left