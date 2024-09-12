-- Vi definerer en funktion der summerer alle tal fra 1 til n
-- Dette gør vi ved hjælp af 2 funktioner
acc :: Num a => [a] -> a

acc [] = 0
acc (n:ns) = n + acc ns


seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)



main :: IO()
main = print()
