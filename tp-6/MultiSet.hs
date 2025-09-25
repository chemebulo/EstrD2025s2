module MultiSet
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where

import Map

data MultiSet a = MS (Map a Int)
    deriving Show
{- INV. REP.:
    * Ninguna.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyMS           O(1)
- addMS             O(N)
- ocurrencesMS      O(N)
- unionMS           O(N^2)
- intersectionMS    O(N^2)
- multiSetToList    O(N^2)

-}

-- ################################################### IMPLEMENTACIÓN ###################################################

emptyMS :: MultiSet a
-- PROPÓSITO: Denota un multiconjunto vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "emptyM" de costo constante.
emptyMS = MS emptyM


addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- PROPÓSITO: Dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
-- COSTO: O(N).
    -- Siendo N la cantidad de pares clave-valor en el map, se utiliza la función "addM" de costo lineal. Es por eso que
    -- el costo total de la función es lineal.  
addMS k (MS m) = MS (addM k m)


ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- PROPÓSITO: Dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
-- COSTO: O(N).
    -- Siendo N la cantidad de pares clave-valor en el map, se utiliza la función "ocurrencesM" de costo lineal. Es por eso 
    -- que el costo total de la función es lineal.  
ocurrencesMS k (MS m) = ocurrencesM k m


unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
-- PROPÓSITO: Dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de claves en ks, la cantidad de pares clave-valor en m1 y la cantidad de pares clave-valor m2,
    -- se utiliza la función "unionM" de costo cuadrático y "keys" de costo lineal. Por eso que el costo total de la función
    -- en el peor caso es (N^2 + N) pero simplificado como cuadrático.
unionMS (MS m1) (MS m2) = MS (unionM (keys m1) m1 m2)


intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
-- PROPÓSITO: Dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de pares clave valor de m1 y m2, se utiliza la función "intersectionM" de costo cuadrático.
    -- Es por eso que el costo total de la función en el peor caso es cuadrático.
intersectionMS (MS m1) (MS m2) = MS (intersectionM (keys m1) m1 m2)


multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
-- PROPÓSITO: Dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de pares clave valor del Map, se utiliza la función "mapToList" de costo cuadrático y "keys" de
    -- costo lineal. Es por eso que el costo total de la función en el peor caso es cuadrático.
multiSetToList (MS m) = mapToList (keys m) m


-- ##################################################### AUXILIARES #####################################################

addM :: Ord a => a -> Map a Int -> Map a Int
-- PROPÓSITO: Dados un elemento y un Map, agrega la clave al Map dado.
-- COSTO: O(N).
    -- Siendo N la cantidad de pares clave-valor en el map, se utiliza la operación "lookupM" y "assocM" de costo lineal. 
    -- Es por eso que el costo total de la función es lineal.  
addM k m = case lookupM k m of
             Just v -> assocM k (v+1) m
             _      -> assocM k 1 m


fromJust :: Maybe a -> a
-- PROPÓSITO: Devuelve el valor del dato dado.
-- PRECONDICIÓN: El valor debe existir.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se realiza pattern matching y se devuelve un valor que se accedió.
fromJust (Just x) = x
fromJust _        = error "Se incumplio la precondicion."


ocurrencesM :: Ord a => a -> Map a Int -> Int
-- PROPÓSITO: Dados un elemento y un Map, describe la cantidad de apariciones de ese elemento en el Map dado.
-- COSTO: O(N).
    -- Siendo N la cantidad de pares clave-valor en el Map, se utiliza la operación "lookupM" de costo lineal. Es por eso que
    -- el costo total de la función es lineal.  
ocurrencesM k m = case lookupM k m of
                    Just v -> v
                    _      -> 0


unionM :: Eq k => [k] -> Map k Int -> Map k Int -> Map k Int
-- PROPÓSITO: Dado una lista de claves y dos Map, devuelve un Map con todos los pares clave-valor de ambos Map.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de claves en ks, la cantidad de pares clave-valor en m1 y la cantidad de pares clave-valor m2,
    -- por cada clave se realizan las operaciones "lookupM" y "assocM" de costo lineal. Por eso que el costo total de la función
    -- en el peor caso es cuadrático.
unionM []     _  m2 = m2
unionM (k:ks) m1 m2 = let v1 = fromJust (lookupM k m1)
                       in case lookupM k m2 of
                            Just v2 -> assocM k (v1+v2) (unionM ks m1 m2) 
                            _       -> assocM k v1 (unionM ks m1 m2) 


intersectionM :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
-- PROPÓSITO: Dado una lista de claves y dos Map, devuelve un Map con todos los pares clave-valor que tengan en común.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de claves en ks, la cantidad de pares clave-valor en m1 y la cantidad de pares clave-valor m2,
    -- por cada clave se realizan las operaciones "lookupM" y "assocM" de costo lineal. Por eso que el costo total de la función
    -- en el peor caso es cuadrático.
intersectionM []     _  _  = emptyM
intersectionM (k:ks) m1 m2 = let v1 = fromJust (lookupM k m1)
                              in case lookupM k m2 of
                                   Just v2 -> assocM k (v1+v2) (intersectionM ks m1 m2) 
                                   _       -> intersectionM ks m1 m2 


mapToList :: Eq a => [a] -> Map a Int -> [(a, Int)]
-- PROPÓSITO: Dado un Map devuelve una lista con todos sus claves y valores.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de claves en ks, por cada clave se realiza la operación "lookupM" de costo lineal. Es por eso
    -- que el costo total de la función en el peor caso es cuadrático.
mapToList []     m = []
mapToList (k:ks) m = let v = fromJust (lookupM k m)
                      in (k, v) : mapToList ks m