data Tree a = Node a (Tree a) (Tree a)
            | Leaf

pathsSum :: Num a => Tree a -> [a]
pathsSUm Leaf = pure 0
pathsSum (Node a lt rt) = concat $ ([(a +)] <*> ) <$> (fmap pathsSum [lt, rt] )

paths :: Tree a -> [ [a] ]
paths Leaf = pure []
paths (Node a lt rt) = concat $ ([(a:)] <*> ) <$> (fmap paths [lt, rt])

ex2 = do
    s <- getLine
    n <- return 3
    putStrLn $ s++ show n
    
ex3 = getLine >>= \s -> return 3 >>= \n -> putStrLn $ s ++ show n

--  fmap Functor f => (a->b) -> f a -> f b
-- (<*) Applicative f => f a -> f b -> f a
-- pure Applivative f => a -> f a

