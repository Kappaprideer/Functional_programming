isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

getElemAtIdx :: ([Int],Int) -> Int
getElemAtIdx (a,b) = last (take b a)

capitalize :: [Char] -> [Char]
capitalize w = a : tail w
            where a = toEnum(fromEnum(head w)-32)

