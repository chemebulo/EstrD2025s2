import Stack
import Set

-- ########################################################################################################### --

-- EJERCICIO 1: Cálculo de costos.

-- EJERCICIO 1.1:

head' :: [a] -> a
-- COSTO: O(1).
head' (x:xs) = x


-- EJERCICIO 1.2:

sumar :: Int -> Int
-- COSTO: O(1).
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1


-- EJERCICIO 1.3:

factorial :: Int -> Int
-- COSTO: O(n).
factorial 0 = 1
factorial n = n * factorial (n-1)


-- EJERCICIO 1.4:

longitud :: [a] -> Int
-- COSTO: O(n).
longitud []     = 0
longitud (x:xs) = 1 + longitud xs


-- EJERCICIO 1.5:

factoriales :: [Int] -> [Int]
-- COSTO: O(n^2).
factoriales []     = []
factoriales (x:xs) = factorial x : factoriales xs


-- EJERCICIO 1.6:

pertenece :: Eq a => a -> [a] -> Bool
-- COSTO: O(n).
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs


-- EJERCICIO 1.7:

sinRepetidosL :: Eq a => [a] -> [a]
-- COSTO: O(n^2).
sinRepetidosL []     = []
sinRepetidosL (x:xs) = if pertenece x xs
                         then sinRepetidosL xs
                         else x : sinRepetidosL xs


-- EJERCICIO 1.8:

append :: [a] -> [a] -> [a]
-- COSTO: O(n).
append []     ys = ys
append (x:xs) ys = x : append xs ys


-- EJERCICIO 1.9:

concatenar :: [String] -> String
-- COSTO: O(n).
concatenar []     = []
concatenar (x:xs) = x ++ concatenar xs


-- EJERCICIO 1.10:

takeN :: Int -> [a] -> [a]
-- COSTO: O(n).
takeN 0 xs     = xs
takeN n []     = []
takeN n (x:xs) = x : takeN (n-1) xs


-- EJERCICIO 1.11:

dropN :: Int -> [a] -> [a]
-- COSTO: O(n).
dropN 0 xs     = xs
dropN n []     = []
dropN n (x:xs) = dropN (n-1) xs


-- EJERCICIO 1.11:

partir :: Int -> [a] -> ([a], [a])
-- COSTO: O(n^2).
partir n xs = (takeN n xs, dropN n xs)


-- EJERCICIO 1.12:

minimo :: Ord a => [a] -> a
-- COSTO: O(n^2).
minimo [x]    = x
minimo (x:xs) = min x (minimo xs)


-- EJERCICIO 1.13:

sacar :: Eq a => a -> [a] -> [a]
-- COSTO: O(n).
sacar n []     = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs


-- EJERCICIO 1.14:

ordenar :: Ord a => [a] -> [a]
-- COSTO: O(n^2).
ordenar [] = []
ordenar xs = let m = minimo xs
             in  m : ordenar (sacar m xs)


-- EJERCICIO 2: Set (Conjunto).