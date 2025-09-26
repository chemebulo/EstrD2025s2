import PriorityQueue

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


-- EJERCICIO 2: Ejercicios con BST.

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show


belongsBST :: Ord a => a -> Tree a -> Bool
-- PROPÓSITO: Dado un BST dice si el elemento pertenece o no al árbol.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se realiza una comparación de costo constante y además 
    -- se recorre solamente una parte del mismo, resultando de costo "log N". Es por eso que el costo total de la función
    -- en el peor caso es "log N".
belongsBST x EmptyT           = False
belongsBST x (NodeT x' ti td) = if x <= x'
                                   then x == x' || belongsBST x ti
                                   else belongsBST x td


insertBST :: Ord a => a -> Tree a -> Tree a
-- PROPÓSITO: Dado un BST inserta un elemento en el árbol.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se realiza una comparación de costo constante y además
    -- se recorre solamente una parte del mismo, resultando de costo "log N". Es por eso que el costo total de la función
    -- en el peor caso es "log N".
insertBST x EmptyT           = NodeT x EmptyT EmptyT
insertBST x (NodeT x' ti td) = if x == x'
                                  then NodeT x' ti td
                                  else if x < x'
                                          then NodeT x' (insertBST x ti) td
                                          else NodeT x' ti (insertBST x td)


deleteBST :: Ord a => a -> Tree a -> Tree a
-- PROPÓSITO: Dado un BST borra un elemento en el árbol.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se realiza una comparación de costo constante, se recorre
    -- solamente una parte del mismo, resultando de costo "log N", y además se utiliza la función "armarBST" de costo "log N". 
    -- Es por eso que el costo total de la función en el peor caso es "log N".
deleteBST _ EmptyT           = EmptyT
deleteBST x (NodeT x' ti td) = if x == x'
                                  then armarBST ti td
                                  else if x < x'
                                          then NodeT x' (deleteBST x ti) td
                                          else NodeT x' ti (deleteBST x td)

armarBST :: Ord a => Tree a -> Tree a -> Tree a
-- PROPÓSITO: Dado dos BST, los junta para poder armar un árbol con ambos.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos del árbol, se utiliza la función "splitMaxBST" de costo "log N". Es por eso que el
    -- costo total de la función en el peor caso es "log N".
armarBST EmptyT td = td
armarBST ti     td = let (x, ti') = splitMaxBST ti
                      in NodeT x ti' td


splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- PROPÓSITO: Dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos del árbol, se recorre solamente una parte del mismo realizando operaciones constantes, 
    -- resultando de costo "log N". Es por eso que el costo total de la función en el peor caso es "log N".
splitMinBST (NodeT x ti EmptyT) = (x, ti)
splitMinBST (NodeT x ti td)     = let (x', ti') = splitMinBST ti
                                   in (x', NodeT x ti' td)


splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- PROPÓSITO: Dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos del árbol, se recorre solamente una parte del mismo realizando operaciones constantes, 
    -- resultando de costo "log N". Es por eso que el costo total de la función en el peor caso es "log N".
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST (NodeT x ti td)     = let (x', td') = splitMaxBST td
                                   in (x', NodeT x ti td')


esBST :: Ord a => Tree a -> Bool
-- PROPÓSITO: Indica si el árbol cumple con los invariantes de BST.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se utiliza la función "sonMenoresQue" y "sonMayoresQue"
    -- de costo lineal. Es por eso que el costo total de la función en el peor caso es cuadrático.
esBST EmptyT          = True
esBST (NodeT x ti td) = sonMenoresQue x ti && sonMayoresQue x td

sonMenoresQue :: Ord a => a -> Tree a -> Bool
-- PROPÓSITO: Describe si el elemento dado es mayor a todos los elementos del árbol dado. 
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se realiza una comparación de costo constante. Es por eso
    -- que el costo total de la función en el peor caso es lineal.
sonMenoresQue x' EmptyT          = True
sonMenoresQue x' (NodeT x ti td) = x < x' && sonMenoresQue x' ti && sonMenoresQue x' td

sonMayoresQue :: Ord a => a -> Tree a -> Bool
-- PROPÓSITO: Describe si el elemento dado es menor a todos los elementos del árbol dado. 
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se realiza una comparación de costo constante. Es por eso
    -- que el costo total de la función en el peor caso es lineal.
sonMayoresQue x' EmptyT          = True
sonMayoresQue x' (NodeT x ti td) = x > x' && sonMayoresQue x' ti && sonMayoresQue x' td


elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
-- PROPÓSITO: Dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se realiza una comparación de costo constante y se recorre
    -- solamente una parte del mismo, resultando de costo "log N". Es por eso que el costo total de la función en el peor caso es "log N".
elMaximoMenorA x' EmptyT          = Nothing
elMaximoMenorA x' (NodeT x ti td) = if x' <= x
                                       then elMaximoMenorA x' ti
                                       else case elMaximoMenorA x' td of
                                               Nothing -> Just x
                                               Just m  -> Just m


elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
-- PROPÓSITO: Dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se realiza una comparación de costo constante y se recorre
    -- solamente una parte del mismo, resultando de costo "log N". Es por eso que el costo total de la función en el peor caso es "log N".
elMinimoMayorA x' EmptyT          = Nothing
elMinimoMayorA x' (NodeT x ti td) = if x' >= x
                                       then elMinimoMayorA x' td
                                       else case elMinimoMayorA x' ti of
                                               Nothing -> Just x
                                               Just m  -> Just m


balanceado :: Tree a -> Bool
-- PROPÓSITO: Indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la diferencia de alturas entre
--            el subárbol izquierdo y el derecho es menor o igual a 1.
-- PRECONDICIÓN: El árbol dado cumple los invariantes de BST y no tiene elementos repetidos.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se utiliza la función "altura" de costo lineal. Es por eso
    -- que el costo total de la función en el peor caso es cuadrático. 
balanceado EmptyT          = True
balanceado (NodeT x ti td) = (abs (altura ti - altura td) <= 1) && balanceado ti && balanceado td

altura :: Tree a -> Int
-- PROPÓSITO: Describe la altura del árbol dado.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos del árbol, por cada elemento se utiliza la función "max" de costo constante. Es por eso
    -- que el costo total de la función en el peor caso es lineal.
altura EmptyT          = 0
altura (NodeT x ti td) = 1 + max (altura ti) (altura td)


-- EJERCICIO 3: Reimplementación de Map con costos logarítmicos.


-- EJERCICIO 4: Empresa.