sqr :: Double -> Double 
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt ( x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x,y,z) = sqrt (x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (x,y) = (y,x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x,y,z) = (x==y) && (y==z)


funkcja identycznościowa id:

id :: a -> a
id x = x 

dowolny swap:

swap :: (a, b) -> (b,a)
swap (a,b) = (b,a)

class Eq a where
(==) :: a -> a -> Bool
(/=)

isEqual :: Eq a => (a,a) -> Bool
isEqual (x,y) = x == y

