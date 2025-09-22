import Set
import QueueV3
import Stack

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
    -- eso que el costo total de la función resulta ser en el peor caso de costo cuadrático.
losQuePertenecen []     s = []
losQuePertenecen (x:xs) s = if belongs x s
                               then x : losQuePertenecen xs s
                               else losQuePertenecen xs s


sinRepetidos :: Eq a => [a] -> [a]
-- PROPÓSITO: Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos, en xs se utiliza la función "sinRepetidosS" de costo cuadrático, y la operación
    -- "setToList" de costo constante. Es por eso que el costo total de la función en el peor caso es cuadrático.
sinRepetidos xs = setToList (sinRepetidosS xs)

sinRepetidosS :: Eq a => [a] -> Set a
-- PROPÓSITO: Describe un Set con todos los elementos de la lista dada. 
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se utiliza la operación "addS" de costo lineal. Es por eso 
    -- que el costo total de la función en el peor caso es cuadrático.
sinRepetidosS []     = emptyS
sinRepetidosS (x:xs) = addS x (sinRepetidosS xs)


unirTodos :: Eq a => Tree (Set a) -> Set a
-- PROPÓSITO: Dado un árbol de conjuntos devuelve un conjunto con la unión de todos los conjuntos del árbol.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos, se realizan las operaciones "unionS" de costo cuadrático. Esto resulta que el costo 
    -- total de la función en el peor caso es (N^2 + N^2), aunque esto es simplificado como cuadrático (N^2).
unirTodos EmptyT          = emptyS
unirTodos (NodeT s ti td) = unionS s (unionS (unirTodos ti) (unirTodos td))


-- EJERCICIO 3: Queue (Cola) - Costos analizados con la primera versión.

{-      COSTO OPERACIONAL DE CADA FUNCIÓN:

---------------------------------------------------
|         QUEUE_V1        |        QUEUE_V2       |
|-------------------------|-----------------------|
|   emptyQ          O(1)  | emptyQ          O(1)  |
|   isEmptyQ        O(1)  | isEmptyQ        O(1)  |
|   enqueue         O(N)  | enqueue         O(1)  |
|   firstQ          O(1)  | firstQ          O(1)  |
|   dequeue         O(1)  | dequeue         O(N)  |
---------------------------------------------------

-}

lengthQ :: Queue a -> Int
-- PROPÓSITO: Cuenta la cantidad de elementos de la cola.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en la Queue, por cada elemento se realizan operaciones de costo constante como "isEmptyQ",
    -- "dequeue". Esto resulta en que el costo total de la función en el peor caso es lineal.
lengthQ q = if not (isEmptyQ q)
               then 1 + lengthQ (dequeue q)
               else 0


queueToList :: Queue a -> [a]
-- PROPÓSITO: Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
-- NOTA: Chequear que los elementos queden en el orden correcto.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en la Queue, por cada elemento se realiza la operación "isEmptyQ", "firstQ" y "dequeue",
    -- todas de costo constante. Es por eso que el costo total de la función en el peor caso es lineal. 
queueToList q = if not (isEmptyQ q)
                   then (firstQ q) : (queueToList (dequeue q))
                   else []


unionQ :: Queue a -> Queue a -> Queue a
-- PROPÓSITO: Inserta todos los elementos de la segunda cola en la primera.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en la Queue, por cada elemento se realizan las operaciones "isEmptyQ", "firstQ" y "dequeue",
    -- todas de costo constante, además de "enqueue" de costo lineal. Es por eso que el costo total de la función en el peor caso es cuadrático.
unionQ q1 q2 = if not (isEmptyQ q2)
                  then enqueue (firstQ q2) (unionQ q1 (dequeue q2))
                  else q1


-- EJERCICIO 4: Stack (Pila).

{-      COSTO OPERACIONAL DE CADA FUNCIÓN:

---------------------------
|          STACK          |
|-------------------------|
|  emptyST          O(1)  |
|  isEmptyST        O(1)  |
|  push             O(N)  |
|  top              O(1)  |
|  pop              O(1)  |
|  lenST            O(1)  |
---------------------------

-}

apilar :: [a] -> Stack a
-- PROPÓSITO: Dada una lista devuelve una pila sin alterar el orden de los elementos.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en el Stack, por cada elemento realiza la operación "push" de costo constante. Es por
    -- eso que el costo total de la función en el peor caso es lineal.
apilar []     = emptyST
apilar (x:xs) = push x (apilar xs)


desapilar :: Stack a -> [a]
-- PROPÓSITO: Dada una pila devuelve una lista sin alterar el orden de los elementos.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en el Stack, por cada elemento realizan las operaciones "isEmptyST", "top" y "pop" de
    -- costo constante. Es por eso que el costo total de la función en el peor caso es lineal.
desapilar st = if not (isEmptyST st)
                  then top st : desapilar (pop st)
                  else []


insertarEnPos :: Int -> a -> Stack a -> Stack a
-- PROPÓSITO: Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha posición (se desapilan elementos
--            hasta dicha posición y se inserta en ese lugar).
-- PRECONDICIÓN: La posicion dada existe dentro del stack dado.
-- COSTO: O(N).
    -- Siendo N la longitud del número n, por cada número se realizan las operaciones "push", "top" y "pop" de costo constante.
    -- Es por eso que el costo total de la función en el peor caso es lineal. 
insertarEnPos 0 x st = push x st
insertarEnPos n x st = push (top st) (insertarEnPos (n-1) x (pop st))


-- EJERCICIO 5: Queue con dos listas.

-- La ventaja es que es eficiente tanto en la operación "enqueue" como "enqueue". Queda evidenciado esto mismo en el gráfico.

{-
---------------------------------------------------------------------------
|         QUEUE_V1        |        QUEUE_V2       |        QUEUE_V3       |         
|-------------------------|-----------------------|-----------------------|
|   emptyQ          O(1)  | emptyQ          O(1)  | emptyQ          O(1)  |
|   isEmptyQ        O(1)  | isEmptyQ        O(1)  | isEmptyQ        O(1)  |
|   enqueue         O(N)  | enqueue         O(1)  | enqueue         O(1)  |
|   firstQ          O(1)  | firstQ          O(1)  | firstQ          O(1)  |
|   dequeue         O(1)  | dequeue         O(N)  | dequeue         O(1)  |
---------------------------------------------------------------------------

-}