--	 2.1
--	[x^2-x | x <- [1..5], x^2-x>3]

--isPrime :: Integral t => t -> Bool
--isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

-- sum[1 | a<-[1..100], b<-[a..100], c<-[b..100], a^2+b^2==c^2]

-- niepoprawna, ponieważ dla 0 oraz 1 zwraca True

isPrime :: Integral t => t -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

-- Napisać ile jest liczb pierwszych w przedziale [1..10000]
-- sum[1 | x<-[1..10000], isPrime x]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]
