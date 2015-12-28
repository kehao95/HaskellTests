module Solution where
-- Definition
data MinQueue a = E -- Empty
				| Node Int a (MinQueue a) (MinQueue a) -- size, element, left, right

empty :: MinQueue a	-- O(1). The empty priority queue.
empty = E

size :: MinQueue a -> Int -- O(1). The number of elements in the queue.
size E  = 0
size q@(Node s e l r) = s

getMin :: MinQueue a ->  Maybe a-- Returns the minimum element of the queue, if the queue is nonempty.
getMin E = Nothing
getMin q@(Node s e l r) = Just e

getVal :: MinQueue a -> a
getVal q@(Node s e l r) = e

getSize :: MinQueue a -> Int
getSize E = 0
getSize q@(Node s e l r) = s

makeNode :: a ->  MinQueue a -> MinQueue a -> MinQueue a
makeNode a q1 q2 = Node (size q1 + size q2 +1) a q1 q2

instance Show (MinQueue a) where
	show E = "E"
	show (Node s e l r) ="("++(show l)++(show s)++(show r)++")" 

deleteMin :: Ord a => MinQueue a -> MinQueue a -- O(log n). Deletes the minimum element. If the queue is empty, does nothing.
deleteMin E = E
deleteMin (Node s _ l E) = Node (s-1) (getVal l) (deleteMin l) E
deleteMin (Node s _ E r) = Node (s-1) (getVal r) E (deleteMin r)
deleteMin (Node s _ l r)
		| vl < vr || (vl==vr && sl>sr)  =  Node (s-1) (getVal l) (deleteMin l) r
		| vl >=vr = Node (s-1) (getVal r) l (deleteMin r)
			where 
				vl = getVal l
				vr = getVal r
				sl = getSize l
				sr = getSize r

insert :: Ord a => a -> MinQueue a -> MinQueue a
insert x E = makeNode x E E
insert x q@(Node s e l r)
	| sl < sr = if x < e then (Node (s+1) x (insert e l) r) else (Node (s+1) e (insert x l) r)
	| sl >= sr = if x < e then (Node (s+1) x l (insert e r)) else (Node (s+1) e l (insert x r)) 
	where 
		vl = getVal l
		vr = getVal r
		sl = getSize l
		sr = getSize r
		
insertR x y = insert y x
fromList :: (Ord a) => [a] -> MinQueue a
fromList = foldl insertR E

toList :: MinQueue a ->[a]-> [a]
toList E l = l


-- test
test = makeNode 1 E E
testlist = fromList [16, 14, 3, 2, 4, 1, 10, 8, 7, 9]
t = Node 3 2 test test
e = E
