module Set
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

-- Implementación sin repetidos y guardando la cantidad de elementos en la estructura.

data Set a = S [a] Int 
    deriving Show
{- INV. REP.:
    * Siendo S xs n:
        * El valor en n siempre debe ser igual a la longitud de xs.
        * Dado un valor x en xs, el mismo no puede existir más de una vez en xs.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyS        O(1)
- addS          O(N)
- belongs       O(N)
- sizeS         O(1)
- removeS       O(N)
- unionS        O(N^2)
- setToList     O(1)

-}

-- ################################################### IMPLEMENTACIÓN ###################################################

emptyS :: Set a
-- PROPÓSITO: Crea un conjunto vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente es un contructor con una lista y un número para inicializar un Set.
emptyS = S [] 0


addS :: Eq a => a -> Set a -> Set a
-- PROPÓSITO: Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se realiza la operacion "==" de costo constante. Esto
    -- porque se utiliza la función "elem" que realiza esto y es de costo lineal en xs. Por eso resulta ser de costo lineal en el peor caso.
addS x (S xs n) = if elem x xs
                     then S xs n
                     else S (x:xs) (n+1) 


belongs :: Eq a => a -> Set a -> Bool
-- PROPÓSITO: Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se realiza la operación "==" de costo constante. Es por
    -- eso que resulta ser de costo lineal en el peor caso.
belongs x (S xs _) = elem x xs


sizeS :: Eq a => Set a -> Int
-- PROPÓSITO: Devuelve la cantidad de elementos distintos de un conjunto.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se realiza pattern matching en el Set dado y se devuelve un campo del mismo.
sizeS (S _ n) = n


removeS :: Eq a => a -> Set a -> Set a
-- PROPÓSITO: Borra un elemento del conjunto.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, se utiliza la función "quitarS" de costo lineal en la cantidad de elementos en xs.
removeS x (S xs n) = let (xs', n') = quitarS x xs n
                      in S xs' n'


unionS :: Eq a => Set a -> Set a -> Set a
-- PROPÓSITO: Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en xs2, se utiliza la función "unirS" de costo cuadrático en xs1 y xs2. Es por eso
    -- que resulta que el costo total de la función en el peor caso sea cuadrático.
unionS (S xs1 n1) (S xs2 _) = let (xs, n) = unirS xs1 xs2 n1
                               in S xs n


setToList :: Eq a => Set a -> [a]
-- PROPÓSITO: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se realiza pattern matching en el Set dado y se devuelve un campo del mismo.
setToList (S xs _) = xs


-- ##################################################### AUXILIARES #####################################################

quitarS :: Eq a => a -> [a] -> Int -> ([a], Int)
-- PROPÓSITO: Describe un par con una lista y un número, después de quitar el elemento dado de la lista dada si el mismo 
--            se encuentra en ella, además de actualizar el número restándole uno, ya que representa el contador de elementos de la lista.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se realiza la operación "==" de costo constante. Es por
    -- eso que resulta de costo lineal en el peor caso.
quitarS x' []     n = ([], n)
quitarS x' (x:xs) n = if x == x'
                         then (xs, n-1)
                         else let (xs', n') = quitarS x' xs n 
                               in (x:xs', n')


unirS :: Eq a => [a] -> [a] -> Int -> ([a], Int)
-- PROPÓSITO: Dadas dos listas, describe un par con una lista y un número, donde la lista representa la unión de las dos
--            listas dadas y el número la cantidad de elementos en la lista resultante.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en ys, por cada elemento se realiza la operación "elem" de costo lineal en ys.
    -- Resultando que el costo total de la función en el peor caso es cuadrático.
unirS xs []     n = (xs, n)
unirS xs (y:ys) n = if elem y xs
                       then unirS xs ys n
                       else let (xs', n') = unirS xs ys n
                             in (y:xs', n'+1)