module Stack
    (Stack, emptyST, isEmptyST, push, top, pop, lenST)
where

data Stack a = S [a] Int
    deriving Show
{- INV. REP.:
    * Siendo S xs n:
        * El valor en "n" debe ser igual a la longitud de elementos en xs.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- emptyST       O(1)
- isEmptyST     O(1)
- push          O(1)
- top           O(1)
- pop           O(1)
- lenST         O(1)

-}

-- ############################################## IMPLEMENTACIÓN ##############################################

emptyST :: Stack a
-- PROPÓSITO: Crea una pila vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza el constructor con una lista y un numero para inicializar un Set.
emptyST = S [] 0


isEmptyST :: Stack a -> Bool
-- PROPÓSITO: Dada una pila indica si está vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se está comparando entre dos valores.
isEmptyST (S _ n) = n == 0


push :: a -> Stack a -> Stack a
-- PROPÓSITO: Dados un elemento y una pila, agrega el elemento a la pila.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza el constructor ":" para agregar el elemento a la lista dada.
push x (S xs n) = S (x:xs) (n+1)


top :: Stack a -> a
-- PROPÓSITO: Dada un pila devuelve el elemento del tope de la pila.
-- PRECONDICIÓN: El Set no puede estar vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se está utilizando la función "head" de costo constante.
top (S xs _) = head xs


pop :: Stack a -> Stack a
-- PROPÓSITO: Dada una pila devuelve la pila sin el primer elemento.
-- PRECONDICIÓN: El Set no puede estar vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se está utilizando la función "tail" de costo constante.
pop (S xs n) = S (tail xs) (n-1)


lenST :: Stack a -> Int
-- PROPÓSITO: Dada la cantidad de elementos en la pila.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se hizo pattern matching para acceder a un campo del Set y devolverlo.
lenST (S _ n) = n


-- ##################################################### AUXILIARES #####################################################