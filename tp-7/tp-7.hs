import PriorityQueue
import Map
import Empresa

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


-- EJERCICIO 3: Recálculo de costos con Map de costos logarítmicos.

{-
    ------------------------
    |        MAP_V4        |
    |----------------------|
    |  emptyM    O(1)      |
    |  assocM    O(log K)  |
    |  lookupM   O(log K)  |
    |  deleteM   O(log K)  |
    |  keys      O(K)      |
    ------------------------
-}

valuesM :: Eq k => Map k v -> [Maybe v]
-- PROPÓSITO: Obtiene los valores asociados a cada clave del Map.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, por cada clave se realiza la operación "keys" de costo lineal, y se utiliza la 
    -- función "valuesM'" de costo "K log K". Es por eso que el costo total de la función en el caso promedio es "K log K + K",
    -- pero se simplifica como K log K.
valuesM m = valuesM' (keys m) m

valuesM' :: Eq k => [k] -> Map k v -> [Maybe v]
-- PROPÓSITO: Describe una lista de valores de las claves dadas.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, por cada clave se utiliza la operación "lookupM" de costo "log K". Es por eso que
    -- el costo total de la función en el caso promedio es "K log K".
valuesM' []     m = []
valuesM' (k:ks) m = lookupM k m : valuesM' ks m


todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- PROPÓSITO: Indica si en el Map se encuentran todas las claves dadas.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, por cada clave se utiliza la función "existeClave" de costo "log K". Es por eso que el
    -- costo total de la función en el caso promedio es "K log K".
todasAsociadas []     m = False
todasAsociadas (k:ks) m = existeClave k m && todasAsociadas ks m

existeClave :: Eq k => k -> Map k v -> Bool
-- PROPÓSITO: Indica si la clave dada se encuentra en el Map dado.
-- COSTO: O(log K).
    -- Siendo K la cantidad de claves del Map, se utiliza la operación "lookupM" de costo "log K". Es por eso que el costo total
    -- de la función en el caso promedio es "log K".
existeClave k m = case lookupM k m of
                    Just _ -> True
                    _      -> False


listToMap :: Eq k => [(k, v)] -> Map k v
-- PROPÓSITO: Convierte una lista de pares clave valor en un Map.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, por cada par de la lista dada se realiza la operación "assocM" de costo "log K".
    -- Es por eso que el costo total de la función en el caso promedio es "K log K".
listToMap []           = emptyM
listToMap ((k, v):kvs) = assocM k v (listToMap kvs)


mapToList :: Eq k => Map k v -> [(k, v)]
-- PROPÓSITO: Convierte un Map en una lista de pares clave valor.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, por cada clave se realiza la operación "keys" de costo lineal, y se utiliza la 
    -- función "mapToList'" de costo "K log K". Es por eso que el costo total de la función en el caso promedio es "K log K + K",
    -- pero se simplifica como K log K.
mapToList m = mapToList' (keys m) m

mapToList' :: Eq k => [k] -> Map k v -> [(k, v)]
-- PROPÓSITO: Convierte un Map en una lista de pares clave valor en base a una lista de claves perteneciente al Map dado.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, por cada clave de la lista dada se utiliza la operación "lookupM" de costo "log K". 
    -- Es por eso que el costo total de la función en el caso promedio es "K log K".
mapToList' []     m = []
mapToList' (k:ks) m = let v = fromJust (lookupM k m)
                       in (k, v) : mapToList' ks m

fromJust :: Maybe a -> a
-- PROPÓSITO: Devuelve el valor del dato dado.
-- PRECONDICIÓN: El valor debe existir.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se realiza pattern matching y se devuelve un valor que se accedió.
fromJust (Just x) = x
fromJust _        = error "Se incumplio la precondicion."


agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- PROPÓSITO: Dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, por cada par de la lista dada se realiza la operación "assocM" y "lookupM" ambas de
    -- costo "log K". Es por eso que el costo total de la función en el caso promedio es "K log K".
agruparEq []           = emptyM
agruparEq ((k, v):kvs) = case lookupM k (agruparEq kvs) of
                           Just vs  -> assocM k (v:vs) (agruparEq kvs)  
                           _        -> assocM k [v] (agruparEq kvs)


incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- PROPÓSITO: Dada una lista de claves de tipo k y un Map que va de k a Int, le suma uno a cada número asociado con
--            dichas claves.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, por cada clave de la lista dada se utiliza la operación "lookupM" y/o "assocM" ambas
    -- de costo "log K". Es por eso que el costo total de la función en el caso promedio es "K log K".
incrementar []     m = emptyM
incrementar (k:ks) m = case lookupM k m of
                         Just v -> assocM k (v+1) (incrementar ks m)  
                         _      -> incrementar ks m


mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
-- PROPÓSITO: Dado dos Maps se agregan las claves y valores del primer Map en el segundo. Si una clave del primero existe
--            en el segundo, es reemplazada por la del primero.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del primer Map, se utiliza la realiza "keys" de costo lineal, y se utiliza la función
    -- "mergeMaps'" de costo "K log K". Es por eso que el costo total de la función en el caso promedio es "K log K".
mergeMaps m1 m2 = mergeMaps' (keys m1) m1 m2

mergeMaps' :: Eq k => [k] -> Map k v -> Map k v -> Map k v
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del primer Map, por cada clave de la lista dada se realizan las operaciones "lookupM" y
    -- "assocM" de costo "log K". Es por eso que el costo total de la función en el caso promedio es "K log K".
mergeMaps' []     _  m2 = m2
mergeMaps' (k:ks) m1 m2 = let v = fromJust (lookupM k m1)
                           in assocM k v (mergeMaps' ks m1 m2) 


indexar :: [a] -> Map Int a
-- PROPÓSITO: Dada una lista de elementos construye un Map que relaciona cada elemento con su posición en la lista.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, con la lista dada se utiliza la función "indexar'" de costo "K log K".
    -- Es por eso que el costo total de la función en el caso promedio es "K log K".
indexar xs = indexar' 0 xs

indexar' :: Int -> [a] -> Map Int a
-- PROPÓSITO: Dada una lista de elementos construye un Map que relaciona cada elemento con su posición en la lista.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del Map, por cada par de la lista dada se realiza la operación "assocM" de costo "log K".
    -- Es por eso que el costo total de la función en el caso promedio es "K log K".
indexar' n []     = emptyM
indexar' n (x:xs) = assocM n x (indexar' (n+1) xs)


ocurrencias :: String -> Map Char Int
-- PROPÓSITO: Dado un string, devuelve un Map donde las claves son los caracteres que aparecen en el string, y los valores
--            la cantidad de veces que aparecen en el mismo.
-- COSTO: O(K log K).
    -- Siendo K la cantidad de claves del , por cada caracter de la lista dada se realizan las operaciones "lookupM" y "assocM"
    -- de costo "log K". Es por eso que el costo total de la función en el caso promedio es "K log K".
ocurrencias []     = emptyM
ocurrencias (c:cs) = case lookupM c (ocurrencias cs) of
                         Just v -> assocM c (v+1) (ocurrencias cs)  
                         _      -> assocM c 1 (ocurrencias cs)


-- EJERCICIO 4: Empresa.

{-
    ---------------------------------------------------
    |                     EMPRESA                     |
    |-------------------------------------------------|
    |  consEmpresa         O(1)                       |
    |  buscarPorCUIL       O(log C)                   |
    |  empleadosDelSector  O(log S + E)               |
    |  todosLosCUIL        O(C)                       |
    |  todosLosSectores    O(S)                       |
    |  agregarSector       O(log S)                   |
    |  agregarEmpleado     O(S log S + log E + log C) |
    |  agregarASector      O(log S + log C + log E)   |
    |  borrarEmpleado      O(S log S + log C + log E) |
    ---------------------------------------------------
-}

-- EJERCICIO 5: Funciones de usuario Empresa.

comenzarCon :: [SectorId] -> [CUIL] -> Empresa
-- PROPÓSITO: Construye una empresa con la información de empleados dada. Los sectores no tienen empleados.
-- COSTO: O(C * (S log S + log C + log E)).
    -- Siendo C la cantidad de CUIL, S la cantidad de Sectores y E la cantidad de Empleados; se utiliza la función "empresaConCUILs" 
    -- de costo "C * (S log S + log C + log E)", y "agregarSectores" de costo "S log S". Es por eso que el costo total de la
    -- función en el caso promedio es "C * (S log S + log C + log E)".
comenzarCon ss cc = agregarSectores ss (empresaConCUILs cc)

empresaConCUILs :: [CUIL] -> Empresa
-- PROPÓSITO: Devuelve una empresa con los CUIL dados.
-- COSTO: O(C * (S log S + log C + log E)).
    -- Siendo C la cantidad de CUIL, S la cantidad de Sectores y E la cantidad de Empleados; por cada CUIL se utiliza la 
    -- operación "agregarEmpleado" de costo "S log S + log C + log E". Es por eso que el costo total de la función en el 
    -- caso promedio es "C * (S log S + log C + log E)".
empresaConCUILs []     = consEmpresa
empresaConCUILs (c:cs) = agregarEmpleado [] c (empresaConCUILs cs)

agregarSectores :: [SectorId] -> Empresa -> Empresa
-- PROPÓSITO: Agrega los SectorId dados a la empresa dada.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de Sectores, por cada sector se utiliza la operación "agregarSector" de costo "log S". Es por eso
    -- que el costo total de la función en el caso promedio es "S log S".
agregarSectores []     e = e
agregarSectores (s:ss) e = agregarSector s (agregarSectores ss e)


recorteDePersonal :: Empresa -> Empresa
-- PROPÓSITO: Dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
-- COSTO: O(C * (S log S + log C + log E)).
    -- Siendo C la cantidad de CUIL, S la cantidad de Sectores y E la cantidad de Empleados; se utiliza la operación "todosLosCUIL"
    -- de costo "E", y la función "recorteDePersonal'" de costo "C * (S log S + log C + log E)". Es por eso que el costo total
    -- de la función en el caso promedio es "C * (S log S + log C + log E)".
recorteDePersonal e = recorteDePersonal' (todosLosCUIL e) e

recorteDePersonal' :: [CUIL] -> Empresa -> Empresa
-- PROPÓSITO: Dada una empresa y su lista de CUILs, elimina a la mitad de sus empleados (sin importar a quiénes).
-- COSTO: O(C * (S log S + log C + log E)).
    -- Siendo C la cantidad de CUIL, S la cantidad de Sectores y E la cantidad de Empleados; por cada CUIL se utiliza la
    -- operación "borrarEmpleado" de costo "S log S + log C + log E". Es por eso que el costo total de la función en el
    -- caso promedio es "C * (S log S + log C + log E)".
recorteDePersonal' []        e = e
recorteDePersonal' (c:[])    e = e
recorteDePersonal' (c:c':cs) e = let e' = recorteDePersonal' cs e
                                  in borrarEmpleado c' e'


convertirEnComodin :: CUIL -> Empresa -> Empresa
-- PROPÓSITO: Dado un CUIL de empleado le asigna todos los sectores de la empresa.
-- COSTO: O(S + log C + log E).
    -- Siendo C la cantidad de CUIL, S la cantidad de Sectores y E la cantidad de Empleados; se utiliza la operación "todosLos
    -- sectores" de costo lineal en S, y "agregarEmpleado" de costo "log S + log C + log E". Es por eso que el costo total
    -- de la función en el caso promedio es "S + log C + log E".
convertirEnComodin c e = agregarEmpleado (todosLosSectores e) c e


esComodin :: CUIL -> Empresa -> Bool
-- PROPÓSITO: Dado un CUIL de empleado indica si el empleado está en todos los sectores.
-- PRECONDICIÓN: El CUIL es de un empleado de la empresa.
-- COSTO: O(S + log C).
    -- Siendo S la cantidad de Sectores y C la cantidad de CUILs, se utilizan las operaciones "sectores" y "todosLosSectores" 
    -- de costo lineal en S, y "buscarPorCUIL" de costo "log C". Es por eso que el costo total de la función en el caso promedio
    -- es "S + S + log C", pero se simplifica en "S + log C".
esComodin c e = (sectores (buscarPorCUIL c e)) == (todosLosSectores e)