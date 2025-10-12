-- EJERCICIO 1: Invariantes de Representación de RAList.

data RAList a = MkR Int (Map Int a) (Heap a)
    deriving Show
{-
* INVARIANTES DE REPRESENTACIÓN:
    * Siendo MRK N MIA HA:
        * N debe ser igual a la cantidad de claves existentes en MIA.
        * Cada valor asociado a un índice (clave) en MIA, debe existir como elemento en HA.
        * Cada elemento en HA debe existir como valor asociado a un índice (clave) en MIA.
-}

----------------------------------------------------------------------------------------------------------------------------------------

-- EJERCICIO 2: Implementación del TAD RAList.

emptyRAL :: RAList a
-- PROPÓSITO: Devuelve una lista vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utilizan las funciones "emptyM" y "emptyH", ambas de costo constante.
emptyRAL = MkR 0 emptyM emptyH

----------------------------------------------------------------------------------------------------------------------------------------

isEmptyRAL :: RAList a -> Bool
-- PROPÓSITO: Indica si la lista está vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "==" de costo constante.
isEmptyRAL (MkR n _ _) = n == 0

----------------------------------------------------------------------------------------------------------------------------------------

lengthRAL :: RAList a -> Int
-- PROPÓSITO: Devuelve la cantidad de elementos.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se abre la estructura y se devuelve uno de sus campos.
lengthRAL (MkR n _ _) = n

----------------------------------------------------------------------------------------------------------------------------------------

get :: Int -> RAList a -> a
-- PROPÓSITO: Devuelve el elemento en el índice dado.
-- PRECONDICIÓN. El índice debe existir.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos en la lista, se utiliza la operación "lookupM" de costo "log K" aunque en este contexto se puede
    -- entender como "log N" ya que recorre claves del Map "MIA". Es por eso que el costo total de la función en el peor caso es "log N".
get i (MkR _ mia _) = case lookupM mia i of 
                        Just x  -> x
                        Nothing -> error "El índice no existe."  

----------------------------------------------------------------------------------------------------------------------------------------

minRAL :: Ord a => RAList a -> a
-- PROPÓSITO: Devuelve el mínimo elemento de la lista.
-- PRECONDICIÓN: La lista no está vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la función "findMin" y "isEmptyH", ambas de costo constante.
minRAL (MkR _ _ ha) = if isEmptyH ha
                         then error "La lista está vacía."
                         else findMin ha

----------------------------------------------------------------------------------------------------------------------------------------

add :: Ord a => a -> RAList a -> RAList a
-- PROPÓSITO: Agrega un elemento al final de la lista.
-- COSTO: O(log N).
    -- Siendo N la cantidad de elementos en la lista, se utilizan las funciones "assocM" de costo "log K" e "insertH" de costo "log H",
    -- ambas en este contexto se pueden entender como "log N", ya que recorren elementos de la lista. Es por eso que el costo total de
    -- la función en el peor caso es "log N".
add x (MkR n mia ha) = let nN = n+1;
                           nMIA = assocM n x mia;
                           nHA = insertH x ha
                        in MkR nN nMIA nHA

----------------------------------------------------------------------------------------------------------------------------------------

elems :: Ord a => RAList a -> [a]
-- PROPÓSITO: Transforma una RAList en una lista, respetando el orden de los elementos.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos en la lista, se utiliza la función "elemsHA" de costo "N log N". Es por eso que el costo total
    -- de la función en el peor caso es "N log N".
elems (MkR _ _ ha) = elemsHA ha

elemsHA :: Ord a => Heap a -> [a]
-- PROPÓSITO: Describe una lista con todos los elementos de la Heap ordenados.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos en la Heap, por cada elemento se utilizan las funciones "isEmptyH" y "findMin", ambas de costo 
    -- constante; además se utiliza "deleteMin" de costo "log K" aunque en este contexto se puede entender como "log N" ya que recorre
    -- elementos en la Heap. Es por eso que el costo total de la función en el peor caso es "N log N". 
elemsHA ha = if isEmptyH ha
                then []
                else findMin ha : (elemsHA (deleteMin ha))

----------------------------------------------------------------------------------------------------------------------------------------

remove :: Ord a => RAList a -> RAList a
-- PROPÓSITO: Elimina el último elemento de la lista.
-- PRECONDICIÓN: La lista no está vacía.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos en la lista, se utilizan las funciones "removeHA" de costo "N log N" y "deleteM" de costo "log K",
    -- aunque en este contexto se puede entender como "log N" ya que recorre elementos en la lista. Además se utiliza la función "lookupM"
    -- de costo "log K" (idem caso deleteM), que se puede entender como "log N". Es por eso que el costo total de la función en el peor
    -- caso es "N log N".
remove (MkR n mia ha) = case lookupM mia (n-1) of
                          Nothing -> error "La lista está vacía."
                          Just x  -> let nN = n-1;
                                         nMIA = deleteM (n-1) mia;
                                         nHA = removeHA x ha
                                      in MkR nN nMIA nHA

removeHA :: Ord a => a -> Heap a -> Heap a
-- PROPÓSITO: Elimina el elemento dado de la Heap dada.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos en la Heap, por cada elemento se utilizan las funciones "isEmptyH" y "findMin", ambas de costo 
    -- constante; además se utiliza "deleteMin" e "insertH", ambas de costo "log K" aunque en este contexto se pueden entender como "log N"
    -- ya que recorren elementos en la Heap. Es por eso que el costo total de la función en el peor caso es "N log N". 
removeHA x ha = if isEmptyH ha
                   then ha
                   else if (findMin ha) == x
                           then deleteMin ha
                           else insertH (findMin ha) (removeHA x (deleteMin ha))

----------------------------------------------------------------------------------------------------------------------------------------

set :: Ord a => Int -> a -> RAList a -> RAList a
-- PROPÓSITO: Reemplaza el elemento en la posición dada.
-- PRECONDICIÓN: El índice debe existir.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos en la lista, se utiliza la función "removeHA" de costo "N log N", "insertH" de costo "log K" aunque
    -- en este contexto se puede entender como "log N" ya que recorre elementos en la lista. Además, se utiliza "assocM" y "lookupM", ambas
    -- de costo "log K" (idem caso insertH), que se pueden entender como "log N". Es por eso que el costo total de la función en el peor
    -- caso es "N log N". 
set i x (MkR n mia ha) = case lookupM mia i of
                           Nothing -> error "El indice no existe."
                           Just y  -> let nN = n;
                                          nMIA = assocM i x mia;
                                          nHA = insertH x (removeHA y ha)
                                       in MkR nN nMIA nHA

----------------------------------------------------------------------------------------------------------------------------------------

addAt :: Ord a => Int -> a -> RAList a -> RAList a
-- PROPÓSITO: Agrega un elemento en la posición dada.
-- PRECONDICIÓN: El índice debe estar entre 0 y la longitud de la lista.
-- OBSERVACIÓN: Cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos en la lista, se utilizan las funciones "assocM", de costo "log K" aunque en este caso se entiende
    -- como "log N" ya que recorre elementos de la lista; además de "moveMIA" de costo "N log N", "insertH" de costo "log H" (idem caso assocM).
    -- Es por eso que el costo total de la función en el peor caso es "N log N".
addAt i x (MkR n mia ha) = if i >= 0 && i <= n
                              then let nN = n+1;
                                       nMIA = assocM i x (moveMIA i n (domM));
                                       nHA = insertH x ha
                                    in MkR nN nMIA nHA
                              else error "El índice no se encuentra entre 0 y la longitud de la lista."

moveMIA :: Ord a => Int -> Int -> Map Int a -> Map Int a
-- PROPÓSITO: Describe el Map dado después de mover una posición todas las claves desde el primer numero hasta el segundo numero dado.
-- PRECONDICIÓN: El índice debe ser un rango de números válido como clave en el map dado. 
-- COSTO: O(N log N).
    -- Siendo N la cantidad de elementos en el Map, por cada elemento se utilizan la funciones "lookupM" y "assocM", ambas de costo "log K"
    -- aunque en este contexto se puede entender como "log N" ya que recorre elementos del Map. Es por eso que el costo total de la función
    -- en el peor caso es "N log N".
moveMIA n1 n2 m = let elem = fromJust (lookupM n2 m)
                   in if n1 == n2
                         then assocM (n2+1) elem m
                         else assocM (n2+1) elem (moveMIA n1 (n2-1) m)

----------------------------------------------------------------------------------------------------------------------------------------

-- EJERCICIO 3: Eficencia de cada subtarea creada.

-- Ya hecho anteriormente.