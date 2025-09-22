module Queue
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

-- Implementación donde se encola por atrás y se desencola por adelante.

data Queue a = Q [a]
    deriving Show
{- INV. REP.:
    * Ninguna.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyQ        O(1)
- isEmptyQ      O(1)
- enqueue       O(N)
- firstQ        O(1)
- dequeue       O(1)

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
-- COSTO: O(N).
    -- Siendo N la cantidad de elementos en xs, se utiliza la función "++" de costo lineal. Es por eso que el costo total de
    -- la función en el peor caso es lineal.
enqueue x (Q xs) = Q (xs ++ [x])


firstQ :: Queue a -> a
-- PROPÓSITO: Dada una cola devuelve el primer elemento de la cola.
-- PRECONDICIÓN: La Queue no puede estar vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "head" sobre la lista.
firstQ (Q xs) = head xs 


dequeue :: Queue a -> Queue a
-- PROPÓSITO: Dada una cola la devuelve sin su primer elemento.
-- PRECONDICIÓN: La Queue no puede estar vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "tail" sobre la lista.
dequeue (Q xs) = Q (tail xs)


-- ##################################################### AUXILIARES #####################################################