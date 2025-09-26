import PriorityQueue
import Map
import MultiSet

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
                   else []


-- EJERCICIO 2: Map (Diccionario).

{-
    --------------------------------------------------------------
    |       MAP_V1     |        MAP_V2       |      MAP_V3       |
    |------------------|---------------------|-------------------|
    |  emptyM    O(1)  |   emptyM    O(1)    |   emptyM    O(1)  |
    |  assocM    O(N)  |   assocM    O(1)    |   assocM    O(N)  |
    |  lookupM   O(N)  |   lookupM   O(N)    |   lookupM   O(N)  |
    |  deleteM   O(N)  |   deleteM   O(N)    |   deleteM   O(N)  |
    |  keys      O(N)  |   keys      O(N^2)  |   keys      O(1)  |
    --------------------------------------------------------------
-}

-- Los costos de las funciones como usuario de Map, estan dados en base a la V1 del TAD.

valuesM :: Eq k => Map k v -> [Maybe v]
-- PROPÓSITO: Obtiene los valores asociados a cada clave del Map.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de pares clave-valor y M la cantidad de claves en el Map, se utiliza la operación "keys" de costo 
    -- lineal y la función "valuesM'"de costo cuadrático. Es por eso que el costo total de la función en el peor caso es N*M.
valuesM m = valuesM' (keys m) m

valuesM' :: Eq k => [k] -> Map k v -> [Maybe v]
-- PROPÓSITO: Describe una lista de valores de las claves dadas.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de pares clave-valor en el Map y M la cantidad de claves en ks, por cada clave se utiliza la operación
    -- "lookupM" de costo lineal. Es por eso que el costo total de la función en el peor caso es N*M.
valuesM' []     m = []
valuesM' (k:ks) m = lookupM k m : valuesM' ks m


todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- PROPÓSITO: Indica si en el Map se encuentran todas las claves dadas.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de pares clave-valor en el Map y M la cantidad de claves en ks, por cada clave se utiliza la función
    -- "existeClave" de costo lineal, y por cada par clave-valor en el Map la operación "lookupM" de costo lineal. Es por eso
    -- que el costo total de la función en el peor caso es N*M.
todasAsociadas []     m = False
todasAsociadas (k:ks) m = existeClave k m && todasAsociadas ks m

existeClave :: Eq k => k -> Map k v -> Bool
-- PROPÓSITO: Indica si la clave dada se encuentra en el Map dado.
-- COSTO: O(N).
    -- Siendo N la cantidad de pares clave-valor en el Map, se utiliza la operación "lookupM" de costo lineal. Es por eso que
    -- el costo total de la función en el peor caso es lineal.
existeClave k m = case lookupM k m of
                    Just _ -> True
                    _      -> False


listToMap :: Eq k => [(k, v)] -> Map k v
-- PROPÓSITO: Convierte una lista de pares clave valor en un Map.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de pares clave-valor en la lista, por cada par se utiliza la operación "assocM" de costo lineal.
    -- Es por eso que el costo total de la función en el peor caso es cuadrático.
listToMap []           = emptyM
listToMap ((k, v):kvs) = assocM k v (listToMap kvs)


mapToList :: Eq k => Map k v -> [(k, v)]
-- PROPÓSITO: Convierte un Map en una lista de pares clave valor.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de pares clave-valor en el Map y M la cantidad de claves en ks, se utiliza la operación "keys" de
    -- costo lineal en el Map, y la función "mapToList'" de costo N*M. Es por eso que el costo total de la función en el
    -- peor caso es (N*M + M), pero se simplifica como N*M.
mapToList m = mapToList' (keys m) m

mapToList' :: Eq k => [k] -> Map k v -> [(k, v)]
-- PROPÓSITO: Convierte un Map en una lista de pares clave valor en base a una lista de claves perteneciente al Map dado.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de pares clave-valor en el Map y M la cantidad de claves en ks, por cada clave se utiliza la operación
    -- "lookupM" de costo lineal en el Map dado, y ":" de costo constante. Es por eso que el costo total de la función 
    -- en el peor caso es N*M.
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
-- COSTO: O(N^2).
    -- Siendo N la cantidad de pares clave-valor de la lista, por cada par se realiza la operación "lookupM" y "assocM" ambas
    -- de costo lineal. Es por eso que el costo total de la función en el peor caso es cuadrático.
agruparEq []           = emptyM
agruparEq ((k, v):kvs) = case lookupM k (agruparEq kvs) of
                           Just vs  -> assocM k (v:vs) (agruparEq kvs)  
                           _        -> assocM k [v] (agruparEq kvs)


incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- PROPÓSITO: Dada una lista de claves de tipo k y un Map que va de k a Int, le suma uno a cada número asociado con
--            dichas claves.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de pares clave-valor en el Map y M la cantidad de claves en ks, por cada clave se utiliza la operación
    -- "lookupM" de costo lineal en el Map dado, y ":" de costo constante. Es por eso que el costo total de la función 
    -- en el peor caso es N*M.
incrementar []     m = emptyM
incrementar (k:ks) m = case lookupM k m of
                         Just v -> assocM k (v+1) (incrementar ks m)  
                         _      -> incrementar ks m


mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
-- PROPÓSITO: Dado dos Maps se agregan las claves y valores del primer Map en el segundo. Si una clave del primero existe
--            en el segundo, es reemplazada por la del primero.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de pares clave-valor en el segundo Map y M la cantidad de claves en ks, se utiliza la operación
    -- "keys" de costo lineal en el primer Map dado, y la función "mergeMaps'" de costo N*M. Es por eso que el costo total 
    -- de la función en el peor caso es (N + N*M), quedando simplificado como N*M.
mergeMaps m1 m2 = mergeMaps' (keys m1) m1 m2

mergeMaps' :: Eq k => [k] -> Map k v -> Map k v -> Map k v
-- COSTO: O(N*M).
    -- Siendo N la cantidad de pares clave-valor en el segundo Map y M la cantidad de claves en ks, por cada clave se utiliza
    -- la operación "lookupM" y "assocM" de costo lineal en el segundo Map dado, y ":" de costo constante. Es por eso que el 
    -- costo total de la función en el peor caso es N*M.
mergeMaps' []     _  m2 = m2
mergeMaps' (k:ks) m1 m2 = let v = fromJust (lookupM k m1)
                           in assocM k v (mergeMaps' ks m1 m2) 


indexar :: [a] -> Map Int a
-- PROPÓSITO: Dada una lista de elementos construye un Map que relaciona cada elemento con su posición en la lista.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de elementos en la lista y M la cantidad de pares clave-valor del Map resultante, se utiliza la
    -- función "indexar'" de costo N*M. Es por eso que el costo total de la función en el peor caso es N*M.
indexar xs = indexar' 0 xs

indexar' :: Int -> [a] -> Map Int a
-- PROPÓSITO: Dada una lista de elementos construye un Map que relaciona cada elemento con su posición en la lista.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de elementos en la lista y M la cantidad de pares clave-valor del Map resultante, por cada elemento
    -- se utiliza la operacipon "assocM" de costo lineal en el Map resultante. Es por eso que el costo total de la función en
    -- el peor caso es N*M.
indexar' n []     = emptyM
indexar' n (x:xs) = assocM n x (indexar' (n+1) xs)


ocurrencias :: String -> Map Char Int
-- PROPÓSITO: Dado un string, devuelve un Map donde las claves son los caracteres que aparecen en el string, y los valores
--            la cantidad de veces que aparecen en el mismo.
-- COSTO: O(N*M).
    -- Siendo N la cantidad de caracteres en el string y M la cantidad de pares clave-valor del Map resultante, por cada caracters
    -- se utiliza la operacipon "lookupM" y "assocM" de costo lineal en el Map resultante. Es por eso que el costo total de la 
    -- función en el peor caso es N*M.
ocurrencias []     = emptyM
ocurrencias (c:cs) = case lookupM c (ocurrencias cs) of
                         Just v -> assocM c (v+1) (ocurrencias cs)  
                         _      -> assocM c 1 (ocurrencias cs)


-- EJERCICIO 3: MultiSet (Multiconjunto).

{- 
    --------------------------------
    |           MULTISET           |
    |------------------------------|
    |   emptyMS           O(1)     |
    |   addMS             O(N)     |
    |   ocurrencesMS      O(N)     |
    |   unionMS           O(N^2)   |
    |   intersectionMS    O(N^2)   |
    |   multiSetToList    O(N^2)   |
    --------------------------------
-}

ocurrencias' :: String -> MultiSet Char
-- PROPÓSITO: Dado un string, devuelve un Map donde las claves son los caracteres que aparecen en el string, y los valores
--            la cantidad de veces que aparecen en el mismo.
    -- COSTO: O(N*M).
    -- Siendo N la cantidad de caracteres en el string y M la cantidad de Sets en el MultiSet, por cada caracter se realiza la
    -- operacion "addMS" de costo lineal. Es por eso que el costo toteal de la función en el peor caso es N*M.
ocurrencias' []     = emptyMS
ocurrencias' (c:cs) = addMS c (ocurrencias' cs) 