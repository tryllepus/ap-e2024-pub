-- Countdown numbers game

-- Vi skal også bruge et input output bibliotek
import System.IO
-- Vi importerer et timing bibliotek der skal hjælpe os medat holde styr på hvor lang det tager at finde en løsning
import System.CPUTime
-- Et bibliotek til numre
import Numeric

-- Vi deklarer en datatype kaldet "Op" som kan være en af de 4 arimetiske operatorer angivet
data Op = Add | Sub | Mul | Div

-- Her definerer vi at operationer skal vises (printes) som de symboler der er angivet
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- Her definerer vi i hvilke tilfælde de forskellige operatorer er tilaldte
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- Her definerer vi hvordan de forskellige operatorer skal anvendes
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Numeriske operationer
-- Her definerer vi en ny datatype som siger at et udtryk (Expr) enten kan være
-- En integer værdi (Val Int) som f.eks. 5
-- Eller det kan være anvendelsen af en operator på to underudtryk som f.eks. Add 2+2 2-2 
data Expr = Val Int | App Op Expr Expr

-- Her definerer vi hvordan et udtryk skal printes
instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                          brak (Val n) = show n
                          brak e = "(" ++ show e ++")"

-- Her definerer vi en funktion som trækker alle integers ud af et udtryk
-- Tager et udtryk og returnerer en liste af integers
values :: Expr -> [Int]
-- Hvis udtrykk kun består af én værdi, returneres der en liste bestående af den værdi
values (Val n) = [n]
-- Hvis udtrykket af en anvendelse af en operator på to udtryk, kaldes funktionen rekursivt på de udtryk og sammenføjer resultatet.
values (App _ l r) = values l ++ values r

-- Her definerer vi en funktion som evaluerer et udtryk
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]