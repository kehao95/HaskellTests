module Solution where
import qualified Data.Set as Set
solution :: Integer -> Integer -> Integer -> Integer
solution m n t = water (Set.fromList [(0,0)]) 0 m n t

water :: (Set.Set (Integer,Integer)) -> Integer-> Integer -> Integer -> Integer -> Integer 
water current step m n target
    | target_in_current = step  -- if target in current return step
    | no_state_change = -1      -- if the current do not change anymore return -1 as false
    | otherwise = water next (step+1) m n target
    where 
        next = pourListToSet (Set.toList current) m n
        target_in_current = targetInList (Set.toList current) target
        no_state_change = Set.null (Set.difference next current)

targetInList :: [(Integer,Integer)] -> Integer -> Bool
targetInList [] target = False
targetInList ((a,b):xs) target =   a == target || b == target || targetInList xs target

pourListToSet :: [(Integer,Integer)] -> Integer -> Integer -> (Set.Set (Integer,Integer)) 
pourListToSet [] m n = Set.fromList [(0,0)]
pourListToSet (state:states) m n = Set.union (Set.fromList (pourAState state m n)) (pourListToSet states m n)

pourAState :: (Integer,Integer) -> Integer -> Integer -> [(Integer,Integer)]
pourAState (l,r) m n= [emptyLeft,emptyRight,fullLeft,fullRight,leftToRight,rightToLeft]
    where 
        emptyLeft   = (0,r)
        emptyRight  = (l,0)
        fullLeft    = (m,r)
        fullRight   = (l,n)
        leftToRight = if l < (n-r) then (0,r+l) else (l+r-n,n )
        rightToLeft = if r < (m-l) then (l+r,0) else (m ,l+r-m)