fib :: (Num a, Eq a) => a -> a
fib n =
    if n ==0 || n==1 then n
    else fib (n-2) + fib (n-1)

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs



prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs



length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs 



or' :: [Bool] -> Bool 
or' [] = False
or' (x:xs) = x || or' xs



and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs



elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) = x==n || elem' n xs



doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = (2*x: doubleAll(xs))



squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = (x^2 : squareAll xs)


selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs)
    |even(x) = x : selectEven xs
    |otherwise = selectEven xs


sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc [] = acc
       loop acc (x:xs) = loop (x + acc) xs
       

sumSquares :: Num a => [a] -> a
sumSquares = git 0
 where git acc []     = acc
       git acc (x:xs) = git (x^2 + acc) xs
       
prod'2 :: Num a => [a] -> a
prod'2 = loop 1
 where loop acc [] = acc
       loop acc (x:xs) = loop (x * acc) xs
       
length'2 :: Num a => [a] -> a
length'2 = loop 0
 where loop acc [] = acc
       loop acc (x:xs) = loop (1+acc) xs
       
