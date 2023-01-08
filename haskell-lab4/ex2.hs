-- product type example (one constructor)		PRODUCT TYPE

-- aliasy dla X i Y
type X = Int
type Y = Int

data CartInt2DVec = MkCartInt2DVec X Y -- konwencja: prefix 'Mk' dla konstruktora 
-- let p12 = MkCartInt2DVec 1 2

-- wypisiwaynie współrzędnej x
-- xCoord p12
xCoord :: CartInt2DVec -> X
xCoord (MkCartInt2DVec x _) = x
-- wypisywanie współrzędnej y
-- yCoord p12
yCoord :: CartInt2DVec -> Y
yCoord (MkCartInt2DVec _ y) = y


-- dzięki dodaniu 'a' Cart2DVec może przyjmować liczby, całkowite, rzeczywiste, Chara, itp. 
data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y
-- przykładowe wywołanie yCoord' $ MkCart2DVec' 5.0 10.0


data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}
-- przykładowe tworzenie zmiennej: let p23 = MkCart2DVec'' {x = 2, y = 3}
-- lub let p23 MkCart2DVec'' 2 3

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y= _}) = xVal
-- odpowiednio x p23

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal 
-- odpowiednio y p23

-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL    = error "head': the empty list has no head!"
head' (Cons x xs) = x
-- przykłady użycia:
-- head' (Cons 1 EmptyL)
-- head' Cons 1
-- head' $ Cons 1 $ Cons 2 EmptyL

-- enum type example (special case of sum type)
data ThreeColors = Blue |
                    White |
                    Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Juliette Binoche"
leadingActor White = "Zbigniw Zamachowski"
leadingActor Red = "Irena Jacob"
-- przykładowe użycie: leadingActor White


-- Zadania:
-- 1.1
data Cart3DVec' a = MkCart3DVec' a a a
xCoord3D' :: Cart3DVec' a -> a
xCoord3D' (MkCart3DVec' xVal _ _ ) = xVal
yCoord3D' :: Cart3DVec' a -> a
yCoord3D' (MkCart3DVec' _ yVal _ ) = yVal
zCoord3D' :: Cart3DVec' a -> a
zCoord3D' (MkCart3DVec' _ _ zVal) = zVal
-- let one = MkCart3DVec' 8 9 10
-- xCoord3D' one


-- 1.2
data Cart3DVec'' a = Cart3DVec'' a a a
xCoord3D'' :: Cart3DVec'' a -> a
xCoord3D'' (Cart3DVec'' xVal _ _ ) = xVal
yCoord3D'' :: Cart3DVec'' a -> a
yCoord3D'' (Cart3DVec'' _ yVal _ ) = yVal
zCoord3D'' :: Cart3DVec'' a -> a
zCoord3D'' (Cart3DVec'' _ _ zVal) = zVal
-- let one = Cart3DVec'' 8 9 10
-- xCoord3D'' one

-- 2.0
data Cart3DVec a = Cart3DVec {x3d :: a, y3d :: a, z3d :: a}
-- let one = Cart3DVec 20 5 0
-- x3d one

-- zad. 6
data Shape = Circle Float | Rectangle Float Float
                
area :: Shape -> Float
area (Circle r ) = pi*r^2
area (Rectangle a b) = a*b
-- przykładowe uzycie: area (Circle 2)

-- zad 7.
data Tree a = EmptyT | Node a (Tree a) (Tree a) deriving Show

rootValue :: Tree a -> a
rootValue EmptyT = error "Tree is empty!"
rootValue (Node root leftTree rightTree) = root

-- zad. 8
data TraficLights = RedColor | OrangeColor | GreenColor

actionFor :: TraficLights -> String
actionFor RedColor = "STOP!"
actionFor OrangeColor = "Be carefule, you should stop or drive."
actionFor GreenColor = "Drive."

-- zad. 9
data DriverAction = Stop | Prepare | Drive deriving Show

actionFor' :: TraficLights -> DriverAction
actionFor' RedColor = Stop
actionFor' OrangeColor = Prepare
actionFor' GreenColor = Drive



