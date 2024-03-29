sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1
            
absInt :: Int -> Int
absInt n = if n<0
   then -n
   else n
   
min2Int :: (Int,Int) -> Int
min2Int (a,b) = if a<b
   then a
   else b
{-
min3Int :: (Int,Int,Int) -> Int
min3Int (a,b,c) = if (a<b) && (a<c)
   then a
   else if (b<c) && (b<c)
      then b
      else c
-}
min3Int :: (Int,Int,Int) -> Int
min3Int (a,b,c) = min2Int(min2Int(a,b),c)

toUpper :: Char -> Char
toUpper x = toEnum(fromEnum(x)-32)

toLower :: Char -> Char
toLower x = toEnum(fromEnum(x)+32)

isDigit :: Char -> Bool
isDigit a = if fromEnum(a)>=48 && fromEnum(a)<58
   then True
   else False
   
charToNum :: Char -> Int
charToNum a = if isDigit(a)
   then toEnum(fromEnum(a)-48)
   else 0
   
romanDigit :: Char -> String
romanDigit a = if charToNum(a)==1
   then "I"
   else if charToNum(a)==2
      then "II"
      else if charToNum(a)==3
         then "III"
         else if charToNum(a)==4
            then "IV"
            else if charToNum(a)==5
               then"V"
               else if charToNum(a)==6
                  then"VI"
                  else if charToNum(a)==7
                     then "VII"
                     else if charToNum(a)==8
                        then "VIII"
                        else if charToNum(a)==9
                           then "IX"
                           else "X"
                
