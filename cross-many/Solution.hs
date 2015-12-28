module Solution where
solution  = cross_many
cross_many :: [Int] -> [a] -> [a] -> ([a], [a])
cross_many indice a b = foo 0 indice a b 

foo :: Int->[Int]->[a]->[a]->([a],[a])
foo index indice a [] = ([],[])
foo index indice [] b = ([],[])
foo index indice a b
    | indice == [] || index < head indice = (head a:fst next,head b:snd next)
    | index > head indice = foo index (tail indice) a b
    | index == head indice = (head b:fst next,head a:snd next)
        where 
            next  = foo (index+1) indice (tail a) (tail b) 
