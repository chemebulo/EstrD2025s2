-- EJERCICIO 1: Tipos Recursivos Simples.

-- EJERCICIO 1.1:

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