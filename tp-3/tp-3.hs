-- EJERCICIO 1: Tipos Recursivos Simples.

-- EJERCICIO 1.1: Celdas con Bolitas.

data Color = Azul | Rojo
    deriving Show

data Celda = Bolita Color Celda | CeldaVacia
    deriving Show


-- EJERCICIO 1.1.1:

nroBolitas :: Color -> Celda -> Int
-- PROPÓSITO: Dados un color y una celda, indica la cantidad de bolitas de ese color. 
-- NOTA: Pensar si ya existe una operación sobre listas que ayude a resolver el problema.
nroBolitas _  CeldaVacia      = 0
nroBolitas c1 (Bolita c2 cel) = unoSi (sonMismoColor c1 c2) + nroBolitas c1 cel

unoSi :: Bool -> Int
-- PROPÓSITO: Describe uno si el valor de verdad dado es verdadero, de lo contrario devuelve 0.
unoSi True  = 1 
unoSi False = 0

sonMismoColor :: Color -> Color -> Bool
-- PROPÓSITO: Indica si ambos colores dados son iguales.
sonMismoColor Rojo Rojo = True
sonMismoColor Azul Azul = True
sonMismoColor _    _    = False


-- EJERCICIO 1.1.2:

poner :: Color -> Celda -> Celda
-- PROPÓSITO: Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner c cel = Bolita c cel


-- EJERCICIO 1.1.3:

sacar :: Color -> Celda -> Celda
-- PROPÓSITO: Dado un color y una celda, quita una bolita de dicho color de la celda. 
-- NOTA: A diferencia de Gobstones, esta función es total.
sacar _  CeldaVacia      = CeldaVacia
sacar c1 (Bolita c2 cel) = if sonMismoColor c1 c2
                              then cel
                              else Bolita c2 (sacar c1 cel)


-- EJERCICIO 1.1.4:

ponerN :: Int -> Color -> Celda -> Celda
-- PROPÓSITO: Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN 0 _ ce = ce
ponerN n c ce = Bolita c (ponerN (n-1) c ce)


-- EJERCICIO 1.2: Camino hacia el Tesoro.

data Objeto = Cacharro | Tesoro
    deriving Show

data Camino = Fin | Nada Camino | Cofre [Objeto] Camino 
    deriving Show


-- EJERCICIO 1.2.1:

hayTesoro :: Camino -> Bool
-- PROPÓSITO: Indica si hay un cofre con un tesoro en el camino.
hayTesoro Fin            = False
hayTesoro (Nada cam)     = hayTesoro cam
hayTesoro (Cofre os cam) = contieneTesoro os ||  hayTesoro cam

contieneTesoro :: [Objeto] -> Bool
-- PROPÓISTO: Indica si hay un tesoro en la lista de objetos dada.
contieneTesoro []     = False
contieneTesoro (o:os) = esTesoro o || contieneTesoro os 

esTesoro :: Objeto -> Bool
-- PROPÓSITO: Indica si el objeto dado es tesoro.
esTesoro Tesoro = True
esTesoro _      = False


-- EJERCICIO 1.2.2:

pasosHastaTesoro :: Camino -> Int
-- PROPÓSITO: Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--            Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- PRECONDICICIÓN: Tiene que haber al menos un tesoro.
pasosHastaTesoro (Nada cam)     = 1 + pasosHastaTesoro cam 
pasosHastaTesoro (Cofre os cam) = if not (contieneTesoro os)
                                     then 1 + pasosHastaTesoro cam
                                     else 0


-- EJERCICIO 1.2.3:

hayTesoroEn :: Int -> Camino -> Bool
-- PROPÓSITO: Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica
--            si hay un tesoro en 5 pasos.
hayTesoroEn 0 (Cofre os _)  = contieneTesoro os
hayTesoroEn 0 _             = False
hayTesoroEn _ Fin           = False
hayTesoroEn n (Nada cam)    = hayTesoroEn (n-1) cam
hayTesoroEn n (Cofre _ cam) = hayTesoroEn (n-1) cam


-- EJERCICIO 1.2.4:

alMenosNTesoros :: Int -> Camino -> Bool
-- PROPÓSITO: Indica si hay al menos "n" tesoros en el camino
alMenosNTesoros 0 _              = True
alMenosNTesoros n Fin            = False
alMenosNTesoros n (Nada cam)     = alMenosNTesoros n cam
alMenosNTesoros n (Cofre os cam) = n <= (cantidadDeTesoros os) || alMenosNTesoros (n - (cantidadDeTesoros os)) cam

cantidadDeTesoros :: [Objeto] -> Int
-- PROPÓSITO: Describe la cantidad de tesoros que hay en la lista de objetos dada.
cantidadDeTesoros []     = 0
cantidadDeTesoros (o:os) = unoSi (esTesoro o) + cantidadDeTesoros os


-- EJERCICIO 1.2.5:

cantTesorosEntre :: Int -> Int -> Camino -> Int
-- PROPÓSITO: Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si el rango es 3 y 5,
--            indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están incluidos tanto 3 como 5 en el resultado.
-- PRECONDICIÓN: Siendo n el primer número dado, y m el segundo: n <= m.
cantTesorosEntre _  _  Fin            = 0
cantTesorosEntre _  0  _              = 0
cantTesorosEntre n1 n2 (Nada cam)     = cantTesorosEntre (n1-1) (n2-1) cam
cantTesorosEntre n1 n2 (Cofre os cam) = if n1 <= 0 && n2 > 0
                                           then cantidadDeTesoros os + cantTesorosEntre n1 (n2-1) cam
                                           else cantTesorosEntre (n1-1) (n2-1) cam


-- ####################################################################################################################### --

-- EJERCICIO 2: Tipos Arbóreos.

-- EJERCICIO 2.1: Árboles binarios.

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show


-- EJERCICIO 2.1.1:

sumarT :: Tree Int -> Int
-- PROPÓSITO: Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT          = 0
sumarT (NodeT n ti td) = n + sumarT ti + sumarT td


-- EJERCICIO 2.1.2:

sizeT :: Tree a -> Int
-- PROPÓSITO: Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT EmptyT          = 0
sizeT (NodeT _ ti td) = 1 + (sizeT ti) + (sizeT td)


-- EJERCICIO 2.1.3:

mapDobleT :: Tree Int -> Tree Int
-- PROPÓSITO: Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n ti td) = NodeT (n*2) (mapDobleT ti) (mapDobleT td)


-- EJERCICIO 2.1.4:

perteneceT :: Eq a => a -> Tree a -> Bool
-- PROPÓSITO: Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT x EmptyT          = False
perteneceT x (NodeT y ti td) = (x == y) || perteneceT x ti || perteneceT x td


-- EJERCICIO 2.1.5:

aparicionesT :: Eq a => a -> Tree a -> Int
-- PROPÓSITO: Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT x EmptyT          = 0
aparicionesT x (NodeT y ti td) = unoSi (x == y) + aparicionesT x ti + aparicionesT x td


-- EJERCICIO 2.1.6:

leaves :: Tree a -> [a]
-- PROPÓSITO: Dado un árbol devuelve los elementos que se encuentran en sus hojas.
-- NOTA: En este tipo se define como hoja a un nodo con dos hijos vacíos.
leaves EmptyT                  = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT _ ti     td)     = leaves ti ++ leaves td


-- EJERCICIO 2.1.7:

heightT :: Tree a -> Int
-- PROPÓSITO: Dado un árbol devuelve su altura.
-- NOTA: La altura de un árbol (height en inglés), también llamada profundidad, es la cantidad de niveles del árbol.
--       La altura para EmptyT es 0, y para una hoja es 1.
heightT EmptyT          = 0
heightT (NodeT _ ti td) = 1 + max (heightT ti) (heightT td)


-- EJERCICIO 2.1.8:

mirrorT :: Tree a -> Tree a
-- PROPÓSITO: Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x ti td) = NodeT x (mirrorT td) (mirrorT ti)


-- EJERCICIO 2.1.9:

toList :: Tree a -> [a]
-- PROPÓSITO: Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- NOTA: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
toList EmptyT          = []
toList (NodeT x ti td) = toList ti ++ [x] ++ toList td


-- EJERCICIO 2.1.10:

levelN :: Int -> Tree a -> [a]
-- PROPÓSITO: Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es la distancia que
--            hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1.
-- NOTA: El primer nivel de un árbol (su raíz) es 0.
levelN _ EmptyT          = []
levelN 0 (NodeT x _  _)  = [x]
levelN n (NodeT _ ti td) = levelN (n-1) ti ++ levelN (n-1) td 


-- EJERCICIO 2.1.11:

listPerLevel :: Tree a -> [[a]]
-- PROPÓSITO: Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel EmptyT          = []
listPerLevel (NodeT x ti td) = [x] : unirListas (listPerLevel ti) (listPerLevel td)

unirListas :: [[a]] -> [[a]] -> [[a]]
-- PROPÓSITO: Describe una lista de listas, resultado de la unión de las listas dadas.
unirListas []       yss      = yss
unirListas xss      []       = xss
unirListas (xs:xss) (ys:yss) = (xs ++ ys) : unirListas xss yss   


-- EJERCICIO 2.1.12:

ramaMasLarga :: Tree a -> [a]
-- PROPÓSITO: Devuelve los elementos de la rama más larga del árbol.
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x ti td) = if heightT ti > heightT td
                                  then x : ramaMasLarga ti
                                  else x : ramaMasLarga td


-- EJERCICIO 2.1.13:

todosLosCaminos :: Tree a -> [[a]]
-- PROPÓSITO: Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos.
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x ti td) = [x] : (agregarA x (todosLosCaminos ti ++ todosLosCaminos td))

agregarA :: a -> [[a]] -> [[a]]
-- PROPÓSITO: Agrega el elemento en todas las listas de las listas de listas dada.
agregarA x []       = []
agregarA x (ys:yss) = (x:ys) : agregarA x yss


-- EJERCICIO 2.2: Expresiones Aritméticas.

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show


-- EJERCICIO 2.2.1:

eval :: ExpA -> Int
-- PROPÓSITO: Dada una expresión aritmética devuelve el resultado evaluarla.
eval (Valor n)     = n
eval (Sum   e1 e2) = (eval e1) + (eval e2)
eval (Prod  e1 e2) = (eval e1) * (eval e2)
eval (Neg   e1)    = (-1) * (eval e1) 

-- EJERCICIO 2.2.2:

simplificar :: ExpA -> ExpA
-- PROPÓSITO: Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando notación matemática convencional):
simplificar (Valor n)     = Valor n
simplificar (Sum   e1 e2) = sumaSimplificada (simplificar e1) (simplificar e2)
simplificar (Prod  e1 e2) = productoSimplificado (simplificar e1) (simplificar e2)
simplificar (Neg   e1)    = negacionSimplificada (simplificar e1) 

sumaSimplificada :: ExpA -> ExpA -> ExpA
-- PROPÓSITO: Describe la expresión aritmetica de la suma simplificada según el criterio descrito.
sumaSimplificada (Valor 0) e2        = e2
sumaSimplificada e1        (Valor 0) = e1
sumaSimplificada e1        e2        = Sum e1 e2

productoSimplificado :: ExpA -> ExpA -> ExpA
-- PROPÓSITO: Describe la expresión aritmetica del producto simplificado según el criterio descrito.
productoSimplificado (Valor 0) _         = Valor 0
productoSimplificado _         (Valor 0) = Valor 0
productoSimplificado (Valor 1) e2        = e2
productoSimplificado e1        (Valor 1) = e1
productoSimplificado e1        e2        = Prod e1 e2

negacionSimplificada :: ExpA -> ExpA
-- PROPÓSITO: Describe la expresión aritmetica de la negación simplificada según el criterio descrito.
negacionSimplificada (Neg e1) = e1
negacionSimplificada e1       = Neg e1