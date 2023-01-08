data BinIntTree = EmptyIntBT | IntNodeBT Int BinIntTree  BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT x leftTree rightTree) = x + sumBinIntTree leftTree + sumBinIntTree rightTree
-- przykładowe użycie: sumBinIntTree $ IntNodeBT 1 (IntNodeBT 2 EmptyIntBT EmptyIntBT) (IntNodeBT 3 EmptyIntBT EmptyIntBT)

data BinTree a = EmptyBT |
                NodeBT a (BinTree a) (BinTree a)
                

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT x lt rt) = x + sumBinTree lt + sumBinTree rt
-- przykładowe użycie: sumBinTree (NodeBT 1 (NodeBT 2 EmptyBT EmptyBT ) (NodeBT 3 EmptyBT EmptyBT ))

data Expr a = Lit a | 
                Add (Expr a) (Expr a) |
                Sub (Expr a) (Expr a) |
                Mul (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n 
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"
-- przykłady użycia: show'(Lit 1)           wynik: 1
-- lub show' (Add (Lit 1) (Lit 2))          wynik: "(1+2)"
-- lub eval (Add (Lit 1) (Lit 2))           wynik 3

-- Zadania:

-- zad. 1
depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT x lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBtPre :: BinTree a -> [a]
flattenBtPre EmptyBT = []
flattenBtPre (NodeBT x lt rt) = [x] ++ flattenBtPre lt ++ flattenBtPre rt

flattenBtIn :: BinTree a -> [a]
flattenBtIn EmptyBT = []
flattenBtIn (NodeBT x lt rt) = flattenBtIn lt ++ [x] ++ flattenBtIn rt

flattenBtPost :: BinTree a -> [a]
flattenBtPost EmptyBT = []
flattenBtPost (NodeBT x lt rt) = flattenBtPost lt ++ flattenBtPost rt ++ [x]

mapBT :: ( a -> b ) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT x lt rt) = NodeBT (f x) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = EmptyBT
insert x (NodeBT node lt rt)
    | node == x = NodeBT node lt rt
    | node < x = NodeBT node lt (insert x rt)
    | node > x = NodeBT node (insert x lt) rt
    
list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

-- zad. 2
occurs :: Eq a => a -> BinTree a -> Int
occurs _ EmptyBT = 0
occurs x (NodeBT node lt rt)
    | x == node = 1 + (occurs x lt) + (occurs x rt)
    | otherwise = (occurs x lt) + (occurs x rt)
    

-- zad. 6



