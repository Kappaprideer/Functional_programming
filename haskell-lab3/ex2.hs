sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a->a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum'' = sumWith id
sumSqr = sumWith sqrt
sumCube = sumWith (^3)
sumAbs = sumWith abs

-- sumWith (^5) [1..15]

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 0
prodWith f (x:xs) = f x * prodWith f xs

prod''    = prodWith id
prodSqr  = prodWith sqrt
prodCube = prodWith (^3)
prodAbs  = prodWith abs
