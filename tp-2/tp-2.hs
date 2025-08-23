-- PUNTO 1: Recursión sobre listas.

-- EJERCICIO 1.1:

sumatoria :: [Int] -> Int
-- PROPÓSITO: Dada una lista de enteros devuelve la suma de to dos sus elementos.
sumatoria []     = 0 
sumatoria (n:ns) = n + sumatoria ns


-- EJERCICIO 1.2:

longitud :: [a] -> Int
-- PROPÓSITO: Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud []     = 0
longitud (_:xs) = 1 + longitud xs  


-- EJERCICIO 1.3:

sucesores :: [Int] -> [Int]
-- PROPÓSITO: Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns 


-- EJERCICIO 1.4:

conjuncion :: [Bool] -> Bool
-- PROPÓSITO: Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion []     = True 
conjuncion (b:bs) = b && conjuncion bs 


-- EJERCICIO 1.5:

disyuncion :: [Bool] -> Bool
-- PROPÓSITO: Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs


-- EJERCICIO 1.6:

aplanar :: [[a]] -> [a]
-- PROPÓSITO: Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar []       = []
aplanar (xs:xss) = xs ++ aplanar xss


-- EJERCICIO 1.7:

pertenece :: Eq a => a -> [a] -> Bool
-- PROPÓSITO: Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs 


-- EJERCICIO 1.8:

apariciones :: Eq a => a -> [a] -> Int
-- PROPÓSITO: Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones e []     = 0
apariciones e (x:xs) = if e == x
                          then 1 + apariciones e xs
                          else apariciones e xs
 

-- EJERCICIO 1.9:

losMenoresA :: Int -> [Int] -> [Int]
-- PROPÓSITO: Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA n []     = []
losMenoresA n (x:xs) = if x < n
                          then x : losMenoresA n xs
                          else losMenoresA n xs


-- EJERCICIO 1.10:

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
-- PROPÓSITO: Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA n []       = []
lasDeLongitudMayorA n (xs:xss) = if length xs > n
                                    then xs : lasDeLongitudMayorA n xss
                                    else lasDeLongitudMayorA n xss 


-- EJERCICIO 1.11:

agregarAlFinal :: [a] -> a -> [a]
-- PROPÓSITO: Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal []     y = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y


-- EJERCICIO 1.12:

agregar :: [a] -> [a] -> [a]
-- PROPÓSITO: Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--            elementos de la segunda a continuación. Definida en Haskell como (++).
agregar []     ys = ys
agregar (x:xs) ys = x : agregar xs ys


-- EJERCICIO 1.13:

reversa :: [a] -> [a]
-- PROPÓSITO: Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa []     = []
reversa (x:xs) = reversa xs ++ [x] 


-- EJERCICIO 1.14:

zipMaximos :: [Int] -> [Int] -> [Int]
-- PROPÓSITO: Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el
--            elemento n de la primera lista y de la segunda lista, teniendo en cuenta que las listas no necesariamente 
--            tienen la misma longitud
zipMaximos []     ys     = ys 
zipMaximos xs     []     = xs 
zipMaximos (x:xs) (y:ys) = if x > y
                              then x : zipMaximos xs ys 
                              else y : zipMaximos xs ys

-- EJERCICIO 1.15:

elMinimo :: Ord a => [a] -> a
-- PROPÓSITO: Dada una lista devuelve el mínimo.
-- PRECONDICIÓN: La lista no es vacía.
elMinimo []     = error "No existe un elemento mínimo de una lista vacía."
elMinimo (x:[]) = x
elMinimo (x:xs) = if x < elMinimo xs
                     then x
                     else elMinimo xs


-- ####################################################################################################################### --

-- PUNTO 2: Recursión sobre números.
