actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >> putChar '\n'

doActSeq = do
    putChar 'A'
    putChar 'G'
    putChar 'H'
    putChar '\n'
    

echo1 = getLine >>= putStrLn

doEcho1 = do
    line <- getLine
    putStrLn line
    
echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
    line <- getLine
    putStrLn $ line ++ "!"
    

echo3 :: IO ()
echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
        >> getLine
        >>= \n -> let num = read n :: Int in
                    if num == 7
                    then putStrLn "ah, lucky 7!"
                    else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"
                        
-- zad. 8.1
echo3' :: IO ()
echo3' = do
       l1 <- getLine
       l2 <- getLine
       putStrLn $ l1 ++ l2
       
dialog' :: IO ()
dialog' = do
        putStr "What is your happy number? "
        n <- getLine
        let number = read n :: Int in
            if number == 7
            then putStrLn "ah, lucky 7!"
            else if odd number
                then putStrLn "Odd number! That's most people's choice..."
                else putStrLn "Hm, even number? Unusual!"
                
-- zad. 8.2
twoQuestions :: IO ()
twoQuestions = do
    putStr "What is your name? "
    name <-getLine
    putStr "How old are you? "
    age <- getLine
    print(name,age)
    
    
twoQuestions' :: IO ()
twoQuestions' = putStr "What is your name? " 
                -- >> getLine 
                >>= \name -> getLine
                >> putStr "How old are you? "
                -- >> getLine
                >>= \age -> getLine
                >> print(name,age)
