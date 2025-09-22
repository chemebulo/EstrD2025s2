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

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show


{-      COSTO OPERACIONAL DE CADA FUNCIÓN:

---------------------------------------------------
|         SET_V1          |         SET_V2        |
|-------------------------|-----------------------|
|   emptyS        O(1)    | emptyS        O(1)    |
|   addS          O(N)    | addS          O(1)    |
|   belongs       O(N)    | belongs       O(N)    |
|   sizeS         O(1)    | sizeS         O(N^2)  |
|   removeS       O(N)    | removeS       O(N)    |
|   unionS        O(N^2)  | unionS        O(N)    |
|   setToList     O(1)    | setToList     O(N^2)  |
---------------------------------------------------

-}

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
-- PROPÓSITO: Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
-- COSTO: O(N^2). Puede ser más eficiente con otra implementación de Set.
    -- Siendo N la cantidad de elementos en xs, por cada elemento se realiza la operación "belongs" de costo lineal. Es por
    -- eso que el costo total de la función resulta ser de costo cuadrático.
losQuePertenecen []     s = []
losQuePertenecen (x:xs) s = if belongs x s
                               then x : losQuePertenecen xs s
                               else losQuePertenecen xs s


sinRepetidos :: Eq a => [a] -> [a]
-- PROPÓSITO: Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos, en xs se utiliza la función "sinRepetidosS" de costo cuadrático, y la operación
    -- "setToList" de costo constante. Es por eso que el costo total de la función es cuadrático.
sinRepetidos xs = setToList (sinRepetidosS xs)

sinRepetidosS :: Eq a => [a] -> Set a
-- PROPÓSITO: Describe un Set con todos los elementos de la lista dada. 
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se utiliza la operación "addS" de costo lineal. Es por eso 
    -- que el costo total de la función es cuadrático.
sinRepetidosS []     = emptyS
sinRepetidosS (x:xs) = addS x (sinRepetidosS xs)


unirTodos :: Eq a => Tree (Set a) -> Set a
-- PROPÓSITO: Dado un árbol de conjuntos devuelve un conjunto con la unión de todos los conjuntos del árbol.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos, se realizan las operaciones "unionS" de costo cuadrático. Esto resulta que el costo 
    -- total de la función es (N^2 + N^2), aunque esto es simplificado como cuadrático (N^2).
unirTodos EmptyT          = emptyS
unirTodos (NodeT s ti td) = unionS s (unionS (unirTodos ti) (unirTodos td))


-- EJERCICIO 3: Queue (Cola).


-- EJERCICIO 4: Stack (Pila).


-- EJERCICIO 5: Queue con dos listas.