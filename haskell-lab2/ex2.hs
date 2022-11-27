fiveToPower :: Integer -> Integer
fiveToPower x=(5^x)

_ToPower5 :: Num a => a -> a
_ToPower5  x = ( x^5)


subtrNFrom5 :: Num a => a -> a 
subtrNFrom5  n = 5-n

subtr5From_ n = n-5

flip2 :: (a->b->c) -> b -> a -> c
flip2 f x y = f y x
