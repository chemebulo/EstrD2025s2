module MapV2
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where 

-- Es la implementación de pares clave-valor con claves repetidas.

data Map k v = M [(k, v)]
    deriving Show
{- INV. REP.:
    * Siendo M kvs -> (k, v):
        * Ninguna.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyM    O(1)
- assocM    O(1)
- lookupM   O(N)
- deleteM   O(N)
- keys      O(N^2)

-}
   
-- ################################################### IMPLEMENTACIÓN ###################################################

emptyM :: Map k v
-- PROPÓSITO: Devuelve un Map vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza el constructor con una lista vacía para inicializar un Map.
emptyM = M []


assocM :: Eq k => k -> v -> Map k v -> Map k v
-- PROPÓSITO: Agrega una asociación clave-valor al Map.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza el constructor ":".
assocM k v (M kvs) = M ((k,v):kvs)


lookupM :: Eq k => k -> Map k v -> Maybe v
-- PROPÓSITO: Encuentra un valor dado una clave.
-- COSTO: O(N).
    -- Siendo N la cantidad de pares clave-valor en kvs, se utiliza la función "buscar" de costo lineal en kvs. Es por eso
    -- que el costo total de la función en el peor caso es lineal.
lookupM k (M kvs) = buscar k kvs


deleteM :: Eq k => k -> Map k v -> Map k v
-- PROPÓSITO: Borra una asociación dada una clave.
-- COSTO: O(N).
    -- Siendo N la cantidad de pares clave-valor en kvs, se utiliza la función "borrar" de costo lineal en kvs. Es por eso
    -- que el costo total de la función en el peor caso es lineal.
deleteM k (M kvs) = M (borrar k kvs)


keys :: Eq k => Map k v -> [k]
-- PROPÓSITO: Devuelve las claves del Map.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de pares clave-valor en kvs, se utiliza la función "claves" de costo cuadrático en kvs. Es por eso
    -- que el costo total de la función en el peor caso es cuadrático.
keys (M kvs) = claves kvs


-- ##################################################### AUXILIARES #####################################################

buscar :: Eq k => k -> [(k, v)] -> Maybe v
-- PROPÓSITO: Describe el valor asociado a la clave en la lista de pares clave-valor dada en caso de existir. En caso
--            contrario devuelve Nothing.
-- COSTO: O(N).
    -- Siendo N la cantidad de pares clave-valor en kvs, por cada par se realiza una comparación con "==" de costo constante.
    -- Es por eso que el costo total de la función en el peor caso es lineal.
buscar k []              = Nothing
buscar k ((k', v'): kvs) = if k == k'
                              then Just v'
                              else buscar k kvs


borrar :: Eq k => k -> [(k, v)] -> [(k, v)]
-- PROPÓSITO: Describe la lista de pares clave-valor después de borar la clave dada.
-- COSTO: O(N).
    -- Siendo N la cantidad de pares clave-valor en kvs, por cada par se realiza una comparación con "==" de costo constante.
    -- Es por eso que el costo total de la función en el peor caso es lineal.
borrar k []             = []
borrar k ((k', v'):kvs) = if k == k'
                             then kvs
                             else (k', v') : (borrar k kvs)


claves :: Eq k => [(k, v)] -> [k]
-- PROPÓSITO: Describe la lista de claves en base a la lista de pares clave-valor dada.
-- COSTO: O(N^2).
    -- Siendo N la cantidad de pares clave-valor en kvs, por cada par se realiza una comparación con "==" de costo constante y
    -- la función "elem" de costo lineal. Es por eso que el costo total de la función en el peor caso es cuadrático.
claves []           = []
claves ((k, v):kvs) = if not (elem k (claves kvs))
                         then k : claves kvs
                         else claves kvs