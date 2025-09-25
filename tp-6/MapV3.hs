module MapV3
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = M [k] [v]
    deriving Show
{- INV. REP.:
    * Siendo M ks vs:
        * No pueden haber keys repetidas en ks.
        * Las listas ks y vs deben tener la misma longitud.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyM    O(1)
- assocM    O(N)
- lookupM   O(N)
- deleteM   O(N)
- keys      O(1)

-}

-- #################################################### IMPLEMENTACIÓN #################################################### 

emptyM :: Map k v
-- PROPÓSITO: Devuelve un Map vacío.
    -- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza el constructor con una lista vacía para inicializar un Map.
emptyM = M [] []


assocM :: Eq k => k -> v -> Map k v -> Map k v
-- PROPÓSITO: Agrega una asociación clave-valor al Map.
-- COSTO: O(N).
    -- Siendo N la cantidad de claves y valores en ks y vs, se utiliza la función "agregar" de costo lineal. Es por eso que
    -- el costo total de la función en el peor caso es lineal.
assocM k v (M ks vs) = let (ks', vs') = agregar k v ks vs
                        in M ks vs


lookupM :: Eq k => k -> Map k v -> Maybe v
-- PROPÓSITO: Encuentra un valor dado una clave.
-- COSTO: O(N).
    -- Siendo N la cantidad de claves y valores en ks y vs, se utiliza la función "valueEn" de costo lineal. Es por eso que
    -- el costo total de la función en el peor caso es lineal.
lookupM k (M ks vs) = valueEn k ks vs


deleteM :: Eq k => k -> Map k v -> Map k v
-- PROPÓSITO: Borra una asociación dada una clave.
-- COSTO: O(N).
    -- Siendo N la cantidad de claves y valores en ks y vs, se utiliza la función "eliminarM" de costo lineal. Es por eso que
    -- el costo total de la función en el peor caso es lineal.
deleteM k (M ks ys) = let (ks, vs) = eliminarM k ks vs
                       in M ks vs


keys :: Map k v -> [k]
-- PROPÓSITO: Devuelve las claves del Map.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se devuelve una lista.
keys (M ks vs) = ks


-- #################################################### AUXILIARES ####################################################

agregar :: Eq k => k -> v -> [k] -> [v] -> ([k],[v])
-- PROPÓSITO: Dado una clave, un valor y una lista de claves y una de valores, agrega la clave dada a la lista de claves y 
--            respectivamente igual para el valor dado.
-- COSTO: O(N).
    -- Siendo N la cantidad de claves y valores en ks y vs, por cada clave y valor se realiza una comparación con "==" de 
    -- costo constante. Es por eso que el costo total de la función en el peor caso es lineal.
agregar k v []       _      = ([k], [v])
agregar k v (k':ks) (v':vs) = if k == k'
                                 then (k':ks, v:vs)
                                 else let (ks', vs') = agregar k v ks vs
                                       in (k:ks', v:vs')


valueEn :: Eq k => k -> [k] -> [v] -> Maybe v
-- PROPÓSITO: Dado una clave, una lista de claves y una lista de valores, devuelve el valor perteneciente a la clave dada.
-- COSTO: O(N).
    -- Siendo N la cantidad de claves y valores en ks y vs, por cada clave y valor se realiza una comparación con "==" de 
    -- costo constante. Es por eso que el costo total de la función en el peor caso es lineal.
valueEn k []       _      = Nothing
valueEn k (k':ks) (v':vs) = if k == k'
                               then Just v'
                               else valueEn k ks vs  


eliminarM :: Eq k => k -> [k] -> [v] -> ([k],[v])
-- PROPÓSITO: Dado una clave, una lista de claves y una lista de valores, devuelve un par de lista de claves y una de valores sin la clave dada.
-- COSTO: O(N).
    -- Siendo N la cantidad de claves y valores en ks y vs, por cada clave y valor se realiza una comparación con "==" de 
    -- costo constante. Es por eso que el costo total de la función en el peor caso es lineal.
eliminarM k [] _            = ([], [])
eliminarM k (k':ks) (v':vs) = if k == k'
                                 then (ks, vs)
                                 else let (ks', vs') = eliminarM k ks vs
                                       in (k:ks', v':vs')