import System.IO

-- Her deklarer vi vores funktion og siger, at den tager en liste af elementer som understøtter ordering
-- Dvs. tal og bogstaver, etc. og returnere en liste tilbage
quickSort :: Ord a => [a] -> [a]
quickSort [] = []

-- Sorteringsprogram
-- Sorterer i stigende orden
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
            where
                smaller = [a | a <- xs, a <= x]
                larger = [b | b <- xs, b > x]

-- fib :: Int -> Int

-- Håndterer ikke negative værdier
-- fib 0 = 1
-- fib 1 = 1
-- fib n = case n of
--     n -> n where n

data Q = Sure | Nope
    deriving (Show)

val :: Q -> Int
-- Pattern matching
val q = case q of
    Sure -> 1
    Nope -> 0

-- data List = Nil | Cons a (List a)
--     deriving (Show)

-- listLength :: List a -> Int
-- listLength Nil = 0
-- listLength (Cons _x xs) = 1 + listLength xs

-- testList :: List Int
-- testList = (Cons 1 (Cons 2 Nil))

-- Phantom type (Unihabited types)
data Void {- Example -}

data Measure unit = Measure Double
    deriving (Show)

data Kilogram
data MeterPerSecond
data Joule

-- double :: Measure a => a -> a
-- double (Measure x) = Measure (x*2)

kinetic :: Kilogram -> MeterPerSecond -> Joule
kinetic (Kilogram m) (MeterPerSecond v) = Joule (0.5 * m * (v ** 2))

mass :: Measure Kilogram
mass = Measure 10

velocity :: Measure MeterPerSecond
velocity = Measure 20

energy :: Measure Kilogram -> Measure MeterPerSecond -> Measure Joule
energy (Measure m) (Measure v) = (0.5 * m * (v ** 2))
-- :set -Wall viser alle advarsler

-- fib n = case n of
--     n < 0 -> undefined
--     n == 0 -> 1
--     n == 1 -> 1
--     otherwise -> fib (n-1) + fib (n-2)

main :: IO ()
main = print (quickSort [6, 5, 4, 6, 10, 54, -1, 9])