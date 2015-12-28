module Solution where
import qualified Data.List
import qualified Data.Char as Char
solution = sum_numbers
sum_numbers :: String -> Int
sum_numbers str = sum [read num :: Int | num <- words str, foldl (&&) True (map Char.isDigit num)]

