-- EJERCICIO 1: Priority Queue (Cola de Prioridad).

{-
    ------------------------
    |    PRIORITY QUEUE    |  
    |----------------------|
    |  emptyPQ       O(1)  |
    |  isEmptyPQ     O(1)  |
    |  insertPQ      O(N)  |
    |  findMinPQ     O(1)  |
    |  deleteMinPQ   O(1)  |
    ------------------------
-}

-- EJERCICIO 1.2:

heapSort :: Ord a => [a] -> [a]
-- PROPÓSITO: Dada una lista la ordena de menor a mayor.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en xs, en la misma se utiliza la función "armarPQ" de costo cuadrático, y "desarmarPQ"
    -- de costo lineal. Es por eso que el costo total de la función en el peor caso es (N^2 + N), simplificado como cuadrático. 
heapSort xs = desarmarPQ (armarPQ xs)

armarPQ :: Ord a => [a] -> PriorityQueue a
-- PROPÓSITO: Describe una Priority Queue en base a la lista dada.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos en xs, por cada elemento se utiliza "insertPQ" de costo lineal. Es por eso que
    -- el costo total de la función en el peor caso es cuadrático. 
armarPQ []     = emptyPQ
armarPQ (x:xs) = insertPQ x (armarPQ xs) 

desarmarPQ :: Ord a => PriorityQueue a -> [a]
-- PROPÓSITO: Describe una lista 
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en la Priority Queue, por cada elemento se utilizan las funciones "findMinPQ" y 
    -- "deleteMinPQ" de costo constante. Es por eso que el costo total de la función en el peor caso es lineal.
desarmarPQ pq = if not (isEmptyPQ pq)
                   then (findMinPQ pq) : desarmarPQ (deleteMinPQ pq) 
                   else emptyPQ


-- EJERCICIO 2: Map (Diccionario).

