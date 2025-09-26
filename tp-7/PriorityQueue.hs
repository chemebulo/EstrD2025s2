module PriorityQueue
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where 

-- Es la implementación donde la lista es ordenada (hay otra donde es arbitraria).

data PriorityQueue a = PQ [a]
    deriving Show
{- INV. REP.:
    * Siendo PQ xs:
        * Los elementos en xs estan ordenados de menor a mayor.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyPQ       O(1)
- isEmptyPQ     O(1)
- insertPQ      O(N)
- findMinPQ     O(1)
- deleteMinPQ   O(1)

-}

-- ################################################### IMPLEMENTACIÓN ###################################################

emptyPQ :: PriorityQueue a
-- PROPÓSITO: Devuelve una Priority Queue vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza el constructor y una lista para inicializar una Priority Queue.
emptyPQ = PQ []


isEmptyPQ :: PriorityQueue a -> Bool
-- PROPÓSITO: Indica si la Priority Queue está vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "null".
isEmptyPQ (PQ xs) = null xs 


insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- PROPÓSITO: Inserta un elemento en la Priority Queue.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, se utiliza la función "insertar" de costo lineal. Es por eso que el costo
    -- total de la función es lineal en el peor caso.
insertPQ x (PQ xs) = PQ (insertar x xs)


findMinPQ :: Ord a => PriorityQueue a -> a
-- PROPÓSITO: Devuelve el elemento más prioriotario (el mínimo) de la Priority Queue.
-- PRECONDICIÓN: Parcial en caso de Priority Queue vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "head".
findMinPQ (PQ xs) = head xs


deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
-- PROPÓSITO: Devuelve una Priority Queue sin el elemento más prioritario (el mínimo).
-- PRECONDICIÓN: Parcial en caso de Priority Queue vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "tail".
deleteMinPQ (PQ xs) = PQ (tail xs)


-- ##################################################### AUXILIARES #####################################################

insertar :: Ord a => a -> [a] -> [a]
-- PROPÓSITO: Inserta el elemento dado en la lista dada en su debido órden.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se comparan los mismos con "==" de costo constante.
    -- Es por eso que el costo total de la función en el peor caso es lineal.
insertar x []       = [x] 
insertar x (x':xs') = if (x <= x')
                         then x : (x':xs')
                         else x' : (insertar x xs') 