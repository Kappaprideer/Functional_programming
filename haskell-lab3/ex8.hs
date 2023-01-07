import Data.Char

doubleElems [] = []
doubleElems (x:xs) = 2 * x : doubleElems xs 

sqrElems [] = []
sqrElems (x:xs) = sqrt x : sqrElems xs

lowerCase [] = []
lowerCase (x:xs) = toLower x : lowerCase xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElems' = map' $ \x -> x*2
sqrElems'    = map' sqrt
lowerCase'   = map' toLower


doubleElems1 xs = [x*2 | x <- xs] 
sqrElems1 xs = [sqrt x | x <- xs]
lowerCase1 xs = [ toLower x | x <- xs]

evalFuncListAt :: a -> [a->b] -> [b]
evalFuncListAt x = map ($x)
