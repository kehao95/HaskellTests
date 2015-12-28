module Solution where
import Data.List

check24 :: Float -> Int
check24 real = if real == 24 then 1 else 0

all_p is = [(a, b, c, d)| a <- is, let l2 = delete a is, b <- l2, let l3 = delete b l2, c <- l3, let d = head (delete c l3)]

cal_turn func1 func2 func3 (a, b, c, d) = [func3 (func2 (func1 a b) c) d, func3 (func1 a (func2 b c)) d, func2 (func1 a b) (func3 c d), func1 a (func3 (func2 b c) d), func1 a (func2 b (func3 c d))]

ops = [(+), (-), (*), (/)]

cals is = [ct | nums <- all_p is, func1 <- ops, func2 <- ops, func3 <- ops, ct <- cal_turn func1 func2 func3 nums]

solution :: [Int] -> Bool
solution is = if sum (map check24 (cals (map fromIntegral is))) > 0 then True else False