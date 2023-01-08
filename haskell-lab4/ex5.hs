newtype MyInt = MkMyInt Int

instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
    
instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <=i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    negate (MkMyInt i )           = MkMyInt (negate i)
    abs (MkMyInt i)               = MkMyInt (abs i)
    signum (MkMyInt i)            = MkMyInt (signum i)
    fromInteger int               = MkMyInt (fromIntegral int)
    
instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i






-- kartkówka

data Tree a = Node (Tree a) a (Tree a) | Leaf a
toList :: Tree a -> [a]
toList (Leaf x)= [x]
toList (Node left x right) = toList left ++ [x] ++ toList right

--------------------------------------------

newtype Box a = MkBox {valueInside :: a}

instance Show a => Show (Box a) where
    show (MkBox v) = "Box with " ++ show v
    
---------------------------------------------

data Data = Data {first:: String, second:: String}

-----------------------------------------------

data Tree' a = Node' (Tree' a) a (Tree' a)
               | Leaf'
               
sumSq :: Num a => Tree' a -> a
sumSq Leaf' = 0
sumSq (Node' left x right) = x^2 + sumSq left + sumSq right

--------------------------------------------------------------------

data Foo a = MkFoo { value :: a, name :: String}

instance Show a => Show (Foo a) where
    show( MkFoo {value = v, name = n} ) = "Foo " ++ show n ++ " with " ++ show v

---------------------------------------------------------------

