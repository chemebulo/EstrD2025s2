module SetV2
    (SetV2, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

-- Implementación con repetidos.

data Set a = S [a]
    deriving Show
{- INV. REP.:
    * Ninguna.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyS        O(1)
- addS          O(1)
- belongs       O(N)
- sizeS         O(N^2)
- removeS       O(N)
- unionS        O(N)
- setToList     O(N^2)

-}

-- ################################################### IMPLEMENTACIÓN ###################################################

emptyS :: Set a
-- PROPÓSITO: Crea un conjunto vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente es un contructor con una lista para inicializar un Set.
emptyS = S []


addS :: Eq a => a -> Set a -> Set a
-- PROPÓSITO: Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "cons" de costo constante entre "x" y "xs".
addS x (S xs) = S (x:xs)


belongs :: Eq a => a -> Set a -> Bool
-- PROPÓSITO: Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se realiza la operación "==" de costo constante. Es por
    -- eso que resulta ser de costo lineal.
belongs x (S xs) = elem x xs


sizeS :: Eq a => Set a -> Int
-- PROPÓSITO: Devuelve la cantidad de elementos distintos de un conjunto.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en xs, se utiliza función "listaSinRepetidos" de costo cuadrático, además de la operación
    -- length de costo lineal. Es por eso que  el costo total de la función es (N^2 + N), aunque se simplifica a (N^2).
sizeS (S xs) = length (listaSinRepetidos xs) 


removeS :: Eq a => a -> Set a -> Set a
-- PROPÓSITO: Borra un elemento del conjunto.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, se utiliza la función "quitarS" de costo lineal en la cantidad de elementos en xs.
removeS x (S xs) = quitarS x xs


unionS :: Eq a => Set a -> Set a -> Set a
-- PROPÓSITO: Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos, se utiliza la operación "++" de costo lineal. Es por eso que la función termina resultando
    -- ser de costo lineal.
unionS (S xs1) (S xs2) = xs1 ++ xs2


setToList :: Eq a => Set a -> [a]
-- PROPÓSITO: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en xs, se utiliza función "listaSinRepetidos" de costo cuadrático. Es por eso que 
    -- el costo total de la función es cuadrático.
setToList (S xs) = listaSinRepetidos xs


-- ##################################################### AUXILIARES #####################################################

listaSinRepetidos :: Eq a => [a] -> [a]
-- PROPÓSITO: Describe la lista resultante de quitarle todos los repetidos a la lista dada.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se utiliza la operación "elem" de costo lineal. Es por eso
    -- que el costo total de la función es cuadrático.s
listaSinRepetidos []     = []
listaSinRepetidos (x:xs) = if elem x listaSinRepetidos xs
                              then listaSinRepetidos xs
                              else x : listaSinRepetidos xs

quitarS :: Eq a => a -> [a] -> [a]
-- PROPÓSITO: Borra el elemento dado de la lista dada si el mismo se encuentra en la misma, todas las veces que lo encuentre.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se realiza la operación "==" de costo constante. Es por eso 
    -- que el costo total de la función es lineal.
quitarS x []     = []
quitarS x (y:ys) = if x == y
                      then quitarS x ys
                      else y : (quitarS x ys)