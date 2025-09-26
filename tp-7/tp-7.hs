-- EJERCICIO 1: Costo HeapSort con implementación de Heap en la PQ.

heapSort :: Ord a => [a] -> [a]
-- PROPÓSITO: Dada una lista la ordena de menor a mayor.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos en la lista y la cantidad de elementos en la PQ, se utilizan las funciones "armarPQ"
    -- de costo "N log N" y "desarmarPQ" de costo "log N". Es por eso que el costo total de la función en el peor caso es "N log N".
heapSort xs = desarmarPQ (armarPQ xs)

armarPQ :: Ord a => [a] -> PriorityQueue a
-- PROPÓSITO: Describe una Priority Queue en base a la lista dada.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos en la lista, por cada elemento se realiza la operación "insertPQ" de costo "log N".
    -- Es por eso que el costo total de la función en el peor caso es "N log N".
armarPQ []     = emptyPQ
armarPQ (x:xs) = insertPQ x (armarPQ xs) 

desarmarPQ :: Ord a => PriorityQueue a -> [a]
-- PROPÓSITO: Describe una lista 
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos en la Priority Queue, por cada elemento se realiza la operación "findMinPQ" y "deleteMinPQ"
    -- de costo constante y "log N" respectivamente. Es por eso que el costo total de la función en el peor caso es "log N".
desarmarPQ pq = if not (isEmptyPQ pq)
                   then (findMinPQ pq) : desarmarPQ (deleteMinPQ pq) 
                   else []


-- EJERCICIO 2:
