module Solution where
import Data.Char
solution::[(Char,Int)]->Int

pointValid ys xps xms x y
    | elem y ys = False
    | elem (x+y) xps = False
    | elem (x-y) xms = False
    | otherwise = True

solve _ _ _ _ 9 = 0
solve xs ys xps xms x
    | x `elem` xs = solve xs ys xps xms (x+1)
    | (8-x) `elem` nex = 9-x
    | nex == [] = solve xs ys xps xms (x+1)
    | otherwise = max (solve xs ys xps xms (x+1)) ((maximum nex) + 1)
    where nex = [solve (x:xs) (y:ys) ((x+y):xps) ((x-y):xms) (x+1)|y<-[1..8],True==(pointValid ys xps xms x y)]


solution ps = solve xs ys (zipWith (\x y->x+y) xs ys) (zipWith (\x y->x-y) xs ys) 1
        where   xs = map (\(x,y)->((ord x)-96)) ps
                ys = map (\(x,y)->(y)) ps