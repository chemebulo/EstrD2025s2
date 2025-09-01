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
ponerN 0 c ce = ce
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
alMenosNTesoros n cam = cantidadDeTesorosEn cam >= n

cantidadDeTesorosEn :: Camino -> Int
-- PROPÓSITO: Describe la cantidad de tesoros que hay en el camino dado.
cantidadDeTesorosEn Fin            = 0
cantidadDeTesorosEn (Nada cam)     = cantidadDeTesorosEn cam
cantidadDeTesorosEn (Cofre os cam) = cantidadDeTesoros os + cantidadDeTesorosEn cam 

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
cantTesorosEntre 0  0  _              = 0
cantTesorosEntre 0  n2 (Nada cam)     = cantTesorosEntre 0 (n2-1) cam 
cantTesorosEntre 0  n2 (Cofre os cam) = cantidadDeTesoros os + cantTesorosEntre 0 (n2-1) cam
cantTesorosEntre n1 n2 (Nada cam)     = cantTesorosEntre (n1-1) (n2-1) cam
cantTesorosEntre n1 n2 (Cofre os cam) = cantTesorosEntre (n1-1) (n2-1) cam


-- ####################################################################################################################### --