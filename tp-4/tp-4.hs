-- EJERCICIO 1: Pizzas.

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show


-- EJERCICIO 1.1:

cantidadDeCapas :: Pizza -> Int
-- PROPÓSITO: Dada una pizza devuelve la cantidad de ingredientes.
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p


-- EJERCICIO 1.2:

armarPizza :: [Ingrediente] -> Pizza
-- PROPÓSITO: Dada una lista de ingredientes construye una pizza.
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)


-- EJERCICIO 1.3:

sacarJamon :: Pizza -> Pizza
-- PROPÓSITO: Le saca los ingredientes que sean jamón a la pizza.
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if esJamon i
                           then sacarJamon p
                           else Capa i (sacarJamon p)

esJamon :: Ingrediente -> Bool
-- PROPÓSITO: Indica si el ingrediente dado es Jamón.
esJamon Jamon = True
esJamon _     = False


-- EJERCICIO 1.4:

tieneSoloSalsaYQueso :: Pizza -> Bool
-- PROPÓSITO: Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En particular, la prepizza,
--            al no tener ningún ingrediente, debería dar verdadero).
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = (esQueso i || esJamon i) && tieneSoloSalsaYQueso p 

esQueso :: Ingrediente -> Bool
-- PROPÓSITO: Indica si el ingrediente dado es Queso.
esQueso Queso = True
esQueso _     = False


-- EJERCICIO 1.5:

duplicarAceitunas :: Pizza -> Pizza
-- PROPÓSITO: Recorre cada ingrediente y si es aceitunas duplica su cantidad.
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = if esAceituna i
                                  then Capa (duplicarAceitunasEn i) (duplicarAceitunas p)
                                  else Capa i (duplicarAceitunas p)

esAceituna :: Ingrediente -> Bool
-- PROPÓSITO: Indica si el ingrediente dado es Aceituna.
esAceituna (Aceitunas _) = True
esAceituna _             = False                                 

duplicarAceitunasEn :: Ingrediente -> Ingrediente
-- PROPÓSITO: Duplica las aceitunas de la dada.
duplicarAceitunasEn (Aceitunas n) = Aceitunas (n*2)


-- EJERCICIO 1.6:

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
-- PROPÓSITO: Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de ingredientes de la pizza,
--           y la respectiva pizza como segunda componente.
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = cantCapasPizza p : cantCapasPorPizza ps

cantCapasPizza :: Pizza -> (Int, Pizza)
-- PROPÓSITO: Dada una pizza, devuelve un par donde la primera componente es la cantidad de ingredientes de la misma, y la respectiva
--            pizza como segunda componente.
cantCapasPizza p = (cantidadDeCapas p, p)


-- ####################################################################################################################### --

-- EJERCICIO 2: Mapa de Tesoros (con bifurcaciones).

data Dir = Izq | Der
    deriving Show

data Objeto = Tesoro | Chatarra
    deriving Show

data Cofre = Cofre [Objeto]
    deriving Show

data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

mapa0 :: Mapa
mapa0 = Bifurcacion (Cofre [Chatarra, Chatarra, Tesoro, Tesoro]) 
            (Bifurcacion (Cofre [Chatarra, Tesoro]) 
                    (Bifurcacion (Cofre[Tesoro])
                        (Fin (Cofre[]))
                        (Fin (Cofre[Tesoro]))
                    )
                    (Fin (Cofre[Tesoro]))
            )
            (Fin (Cofre[])) 


-- EJERCICIO 2.1:

hayTesoro :: Mapa -> Bool
-- PROPÓSTIO: Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Fin c)               = hayTesoroEnC c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnC c || hayTesoro m1 || hayTesoro m2

hayTesoroEnC :: Cofre -> Bool
-- PROPÓSITO: Indica si hay un tesoro en el cofre dado.
hayTesoroEnC (Cofre os) = hayTesoroEnOS os

hayTesoroEnOS :: [Objeto] -> Bool
-- PROPÓSITO: Indica si hay un tesoro en la lista de objetos dada.
hayTesoroEnOS []     = False
hayTesoroEnOS (o:os) = esTesoro o || hayTesoroEnOS os

esTesoro :: Objeto -> Bool
-- PROPÓSITO: Indica si el objeto dado es un tesoro.
esTesoro Tesoro = True
esTesoro _      = False


-- EJERCICIO 2.2:

hayTesoroEn :: [Dir] -> Mapa -> Bool
-- PROPÓSTIO: Indica si al final del camino hay un tesoro. 
-- NOTA: El final de un camino se representa con una lista vacía de direcciones.
hayTesoroEn _      (Fin c)               = hayTesoroEnC c
hayTesoroEn []     (Bifurcacion c _ _)   = hayTesoroEnC c
hayTesoroEn (d:ds) (Bifurcacion _ m1 m2) = case d of    
                                             Izq -> hayTesoroEn ds m1
                                             Der -> hayTesoroEn ds m2


-- EJERCICIO 2.3:

caminoAlTesoro :: Mapa -> [Dir]
-- PROPÓSTIO: Indica el camino al tesoro. 
-- PRECONDICIÓN: Existe un tesoro y es único.
caminoAlTesoro (Fin _)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnC c 
                                          then []
                                          else if hayTesoro m1
                                                  then Izq : caminoAlTesoro m1
                                                  else Der : caminoAlTesoro m2


-- EJERCICIO 2.4:

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- PROPÓSTIO: Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin c)               = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = let caminoRamaIzq = caminoDeLaRamaMasLarga m1;
                                                   caminoRamaDer = caminoDeLaRamaMasLarga m2
                                                in if length caminoRamaIzq > length caminoRamaDer
                                                      then Izq : caminoRamaIzq
                                                      else Der : caminoRamaDer 


-- EJERCICIO 2.5:

tesorosPorNivel :: Mapa -> [[Objeto]]
-- PROPÓSTIO: Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c)               = [tesorosEn c]
tesorosPorNivel (Bifurcacion c m1 m2) = tesorosEn c : unirListas (tesorosPorNivel m1) (tesorosPorNivel m2)

tesorosEn :: Cofre -> [Objeto]
-- PROPÓSITO: Describe los tesoros que hay en el cofre dado.
tesorosEn (Cofre os) = tesorosEnOS os

tesorosEnOS :: [Objeto] -> [Objeto]
-- PROPÓSITO: Describe los tesoros que hay en la lista de objetos dada.
tesorosEnOS []     = []
tesorosEnOS (o:os) = if esTesoro o
                        then o : tesorosEnOS os
                        else tesorosEnOS os

unirListas :: [[a]] -> [[a]] -> [[a]]
-- PROPÓSITO: Describe una lista de listas, resultado de la unión de las listas dadas.
unirListas []       yss      = yss
unirListas xss      []       = xss
unirListas (xs:xss) (ys:yss) = (xs ++ ys) : unirListas xss yss   


-- EJERCICIO 2.6:

todosLosCaminos :: Mapa -> [[Dir]]
-- PROPÓSTIO: Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin _)               = []
todosLosCaminos (Bifurcacion _ m1 m2) = ([Izq] : agregarA Izq (todosLosCaminos m1)) ++ 
                                        ([Der] : agregarA Der (todosLosCaminos m2))

agregarA :: a -> [[a]] -> [[a]]
-- PROPÓSITO: Agrega el elemento en todas las listas de las listas de listas dada.
agregarA x []       = []
agregarA x (ys:yss) = (x:ys) : agregarA x yss

-- ####################################################################################################################### --

-- EJERCICIO 3: Nave Espacial.

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show


-- EJERCICIO 3.1:



-- EJERCICIO 3.2:



-- EJERCICIO 3.3:



-- EJERCICIO 3.4:



-- EJERCICIO 3.5:



-- EJERCICIO 3.6:



-- EJERCICIO 3.7:



-- ####################################################################################################################### --

-- EJERCICIO 4: Manada de Lobos.



-- EJERCICIO 4.1:



-- EJERCICIO 4.2:



-- EJERCICIO 4.3:



-- EJERCICIO 4.4:



-- EJERCICIO 4.5:



-- EJERCICIO 4.6: