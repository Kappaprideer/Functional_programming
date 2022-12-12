onlyEven [] = []
onlyEven (x:xs)
    | x `mod` 2 == 0 = x : onlyEven xs
    | otherwise     = onlyEven xs
    
onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 == 1 = x : onlyOdd xs
    | otherwise = onlyOdd xs
    
onlyUpper [] = []
onlyUpper (x:xs)
    | (x>='A' && x<='Z') = x : onlyUpper xs
    | otherwise = onlyUpper xs
    

filter' :: (a->Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
    | p x == True = x : filter' p xs
    | otherwise     = filter' p xs
    
onlyEven' = filter' even    -- filter' (\x -> `mod` 2 == 0)
onlyOdd' = filter' odd      -- filter' (\x -> x `mod` 2 == 1)
onlyUpper' = filter' (\x -> (x>='A' && x<='Z'))

onlyEven1 = filter'  even 
onlyOdd1 = filter' odd
onlyUpper1 = filter' $ \x -> x>='A' && x<='Z'

ex5 = length ( filter even [1..10^6])
ex5'= length [x | x <- [1..10^6], even x]
