module Queue
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

-- Implementación donde se encola por adelante y se desencola por atrás.

data Queue a = Q [a]
    deriving Show
{- INV. REP.:
    * Ninguna.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyQ        O(1)
- isEmptyQ      O(1)
- enqueue       O(1)
- firstQ        O(1)
- dequeue       O(N)

-}

-- ################################################### IMPLEMENTACIÓN ###################################################

emptyQ :: Queue a
-- PROPÓSITO: Crea una cola vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza el constructor con una lista para inicializar una Queue.
emptyQ = Q []


isEmptyQ :: Queue a -> Bool
-- PROPÓSITO: Dada una cola indica si la cola está vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la función "null" de costo constante.
isEmptyQ (Q xs) = null xs


enqueue :: a -> Queue a -> Queue a
-- PROPÓSITO: Dados un elemento y una cola, agrega ese elemento a la cola.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza el constructor ":" para agregar el elemento a la lista de la Queue.
enqueue x (Q xs) = Q (x:xs)


firstQ :: Queue a -> a
-- PROPÓSITO: Dada una cola devuelve el primer elemento de la cola.
-- PRECONDICIÓN: La Queue no puede estar vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "head" sobre la lista.
firstQ (Q xs) = head xs 


dequeue :: Queue a -> Queue a
-- PROPÓSITO: Dada una cola la devuelve sin su primer elemento.
-- PRECONDICIÓN: La Queue no puede estar vacía.
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, se utiliza la función "init" de costo lineal. Es por eso que el costo total de
    -- la función en el peor caso es lineal.
dequeue (Q xs) = Q (init xs)


-- ##################################################### AUXILIARES #####################################################