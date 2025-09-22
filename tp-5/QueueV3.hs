module QueueV3
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

-- Implementación donde hay dos listas: fs y bs. Se desencola por fs, y se encola por bs.

data Queue a = Q [a] [a]
    deriving Show
{- INV. REP.:
    * Siendo Q fs bs:
        * Si fs se encuenta vacía, entonces la cola se encuentra vacía.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyQ        O(1)
- isEmptyQ      O(1)
- enqueue       O(1)
- firstQ        O(1)
- dequeue       O(1)

-}

-- ################################################### IMPLEMENTACIÓN ###################################################

emptyQ :: Queue a
-- PROPÓSITO: Crea una cola vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza el constructor con dos listas para inicializar una Queue.
emptyQ = Q [] []


isEmptyQ :: Queue a -> Bool
-- PROPÓSITO: Dada una cola indica si la cola está vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la función "null" de costo constante.
isEmptyQ (Q fs bs) = null fs


enqueue :: a -> Queue a -> Queue a
-- PROPÓSITO: Dados un elemento y una cola, agrega ese elemento a la cola.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la función "null" de costo constante.
enqueue x (Q fs bs) = if null fs
                         then Q (x:fs) bs
                         else Q fs (x:bs)


firstQ :: Queue a -> a
-- PROPÓSITO: Dada una cola devuelve el primer elemento de la cola.
-- PRECONDICIÓN: La Queue no puede estar vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "head" sobre la lista.
firstQ (Q fs _) = head fs


dequeue :: Queue a -> Queue a
-- PROPÓSITO: Dada una cola la devuelve sin su primer elemento.
-- PRECONDICIÓN: La Queue no puede estar vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "case" para realizar pattern matching, además de
    -- la operación "tail" y "reverse", lo cual esta última es de costo lineal pero queda amortizado ya que es el caso menos
    -- común. Por ende, el costo total de la función en el caso promedio es constante.
dequeue (Q fs bs) = case fs of
                      (f:[]) -> case bs of
                                  [] -> (Q [] [])
                                  _  -> (Q (reverse bs) [])
                      _      -> (Q (tail fs) bs)

-- ##################################################### AUXILIARES #####################################################