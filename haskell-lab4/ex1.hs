polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r* cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

personInfoToString :: (String, String, String) -> String
personInfoToString (nm, snm, addr) =
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

-- sposób użycia funkcji:
-- personInfoToString ("Michail", "Berlioz", "ul. Sadowa 302a, m.50")
    

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm, snm, addr) =
    "Name: " ++ nm ++ ",    Surname: " ++ snm ++ ",    Address: " ++ addr
    
-- sposób użycia funkcji:
-- personInfoToString' ("Michail", "Berlioz", "ul. Sadowa 302a, m.50")
    

newtype Name'' a = MkName a deriving(Show)
newtype Surname'' a = MkSurname a deriving(Show)
newtype Address'' a = MkAddress a deriving(Show)
type PersonInfo'' = (Name'' String, Surname'' String, Address'' String)

personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (MkName nm, MkSurname srm, MkAddress addr) =
    "Name: " ++ nm ++ ",    Surname: " ++ srm ++ ",     Address: " ++ addr

-- sposób użycia funkcji: 
-- personInfoToString'' (MkName "Adam", MkSurname "Lipski", MkAddress "ul. Sadowa 302a, m.50")

