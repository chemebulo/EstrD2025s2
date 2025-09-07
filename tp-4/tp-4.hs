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

nave0 :: Nave
nave0 = N (NodeT (S "a920" [LanzaTorpedos, (Almacen [Torpedo, Oxigeno])] ["emilia", "enrique", "enriqueta"]) 
                (NodeT (S "34b0" [(Almacen [Torpedo, Comida, Comida]), (Motor 120)] ["enriqueta", "lewis"]) EmptyT EmptyT)
                (NodeT (S "890ab" [(Almacen [Combustible, Comida]), (Motor 450)] ["emilia", "enriqueta", "xing"]) EmptyT EmptyT))

-- EJERCICIO 3.1:

sectores :: Nave -> [SectorId]
-- PROPÓSITO: Devuelve todos los sectores de la nave.
sectores (N ts) = sectoresDeTS ts

sectoresDeTS :: Tree Sector -> [SectorId]
-- PROPÓSITO: Describe todos los sectores del árbol dado.
sectoresDeTS EmptyT            = []
sectoresDeTS (NodeT s tsi tsd) = sectorIDS s : (sectoresDeTS tsi ++ sectoresDeTS tsd)

sectorIDS :: Sector -> SectorId
-- PROPÓSITO: Describe el SectorId del Sector dado.
sectorIDS (S sid _ _) = sid


-- EJERCICIO 3.2:

poderDePropulsion :: Nave -> Int
-- PROPÓSITO: Devuelve la suma de poder de propulsión de todos los motores de la nave. 
-- NOTA: El poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N ts) = poderDePropulsionDeTS ts

poderDePropulsionDeTS :: Tree Sector -> Int
-- PROPÓSITO: Describe la suma de poder de propulsión de todos los motores del árbol dado.
poderDePropulsionDeTS EmptyT            = 0
poderDePropulsionDeTS (NodeT s tsi tsd) = poderDePropulsionDeS s + poderDePropulsionDeTS tsi + poderDePropulsionDeTS tsd

poderDePropulsionDeS :: Sector -> Int
-- PROPÓSITO: Describe la suma de poder de propulsión del sector dado.
poderDePropulsionDeS (S _ cs _) = poderDePropulsionDeCS cs

poderDePropulsionDeCS :: [Componente] -> Int
-- PROPÓSITO: Describe la suma de poder de propulsión de la lista de componentes dada.
poderDePropulsionDeCS []     = 0
poderDePropulsionDeCS (c:cs) = poderDePropulsionDeC c + poderDePropulsionDeCS cs

poderDePropulsionDeC :: Componente -> Int
-- PROPÓSITO: Describe el poder de propulsión del componente dado.
poderDePropulsionDeC (Motor n) = n
poderDePropulsionDeC _         = 0


-- EJERCICIO 3.3:

barriles :: Nave -> [Barril]
-- PROPÓSITO: Devuelve todos los barriles de la nave.
barriles (N ts) = barrilesDeTS ts

barrilesDeTS :: Tree Sector -> [Barril]
-- PROPÓSITO: Describe todos los barriles del árbol dado.
barrilesDeTS EmptyT            = []
barrilesDeTS (NodeT s tsi tsd) = barrilesDeS s ++ barrilesDeTS tsi ++ barrilesDeTS tsd

barrilesDeS :: Sector -> [Barril]
-- PROPÓSITO: Describe todos los barriles del sector dado.
barrilesDeS (S _ cs _) = barrilesDeCS cs

barrilesDeCS :: [Componente] -> [Barril]
-- PROPÓSITO: Describe todos los barriles de la lista de componentes dada.
barrilesDeCS []     = []
barrilesDeCS (c:cs) = barrilesDeC c ++ barrilesDeCS cs

barrilesDeC :: Componente -> [Barril]
-- PROPÓSITO: Describe el barril del componente dado (si es que es un almacen).
barrilesDeC (Almacen bs) = bs
barrilesDeC _            = []


-- EJERCICIO 3.4:

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- PROPÓSITO: Añade una lista de componentes a un sector de la nave.
-- NOTA: Ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector cs sid (N ts) = N (agregarASectorTS cs sid ts)

agregarASectorTS :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
-- PROPÓSITO: Añade una lista de componentes a un sector de la nave.
agregarASectorTS _  _   EmptyT            = EmptyT
agregarASectorTS cs sid (NodeT s tsi tsd) = if (sectorIDS s) == sid
                                               then NodeT (agregarASectorS cs s) tsi tsd
                                               else NodeT s (agregarASectorTS cs sid tsi) (agregarASectorTS cs sid tsd)

agregarASectorS :: [Componente] -> Sector -> Sector
-- PROPÓSITO: Añade una lista de componentes al Sector dado.
agregarASectorS cs1 (S sid cs2 ts) = S sid (cs2 ++ cs1) ts


-- EJERCICIO 3.5:

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- PROPÓSITO: Incorpora un tripulante a una lista de sectores de la nave.
-- PRECONDICIÓN: Todos los id de la lista existen en la nave.
asignarTripulanteA t sids (N ts) = N (asignarTripulanteATS t sids ts)

asignarTripulanteATS :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
-- PROPÓSITO: Incorpora el tripulante dado a una lista de sectores del árbol dado.
asignarTripulanteATS _ _    EmptyT            = EmptyT
asignarTripulanteATS t sids (NodeT s tsi tsd) = if elem (sectorIDS s) sids
                                                   then NodeT (asignarTripulanteAS t s) (asignarTripulanteATS t sids tsi) 
                                                                                        (asignarTripulanteATS t sids tsd)
                                                   else NodeT s (asignarTripulanteATS t sids tsi) 
                                                                (asignarTripulanteATS t sids tsd)

asignarTripulanteAS :: Tripulante -> Sector -> Sector
-- PROPÓSITO: Incorpora el tripulante dado al sector dado.
asignarTripulanteAS t (S sid cs ts) = S sid cs (t:ts)


-- EJERCICIO 3.6:

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- PROPÓSITO: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados t (N ts) = sectoresAsignadosTS t ts

sectoresAsignadosTS :: Tripulante -> Tree Sector -> [SectorId]
-- PROPÓSITO: Describe todos los sectores en donde aparece el tripulante dado.
sectoresAsignadosTS t EmptyT            = []
sectoresAsignadosTS t (NodeT s tsi tsd) = if estaTripulanteEnS t s
                                             then sectorIDS s : (sectoresAsignadosTS t tsi ++ sectoresAsignadosTS t tsd)
                                             else sectoresAsignadosTS t tsi ++ sectoresAsignadosTS t tsd

estaTripulanteEnS :: Tripulante -> Sector -> Bool
-- PROPÓSITO: Indica si el tripulante dado se encuentra en el sector dado.
estaTripulanteEnS t (S _ _ ts) = elem t ts


-- EJERCICIO 3.7:

tripulantes :: Nave -> [Tripulante]
-- PROPÓSITO: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes (N ts) = tripulantesTS ts

tripulantesTS :: Tree Sector -> [Tripulante]
-- PROPÓSITO: Describe la lista de tripulantes que se encuentran en el árbol dado.
tripulantesTS EmptyT            = []
tripulantesTS (NodeT s tsi tsd) = unionSinRepeticiones (tripulantesS s) (unionSinRepeticiones (tripulantesTS tsi) (tripulantesTS tsd))

tripulantesS :: Sector -> [Tripulante]
tripulantesS (S _ _ ts) = ts

unionSinRepeticiones :: Eq a => [a] -> [a] -> [a]
-- PROPÓSITO: Une los elementos de la primera lista a los de la segunda sin repetir.
unionSinRepeticiones []     ys = ys  
unionSinRepeticiones xs     [] = xs
unionSinRepeticiones (x:xs) ys = if elem x ys
                                    then unionSinRepeticiones xs ys
                                    else x : unionSinRepeticiones xs ys 


-- ####################################################################################################################### --

-- EJERCICIO 4: Manada de Lobos.

type Presa      = String -- Nombre de presa.
type Territorio = String -- Nombre de territorio.
type Nombre     = String -- Nombre de lobo.

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
    deriving Show

data Manada = M Lobo
    deriving Show


-- EJERCICIO 4.1:

loberioFeroz :: Manada
loberioFeroz = M (Cazador "Colmillonatan" ["Despistadeo", "Perezosandra", "Papanatalia"]
                    (Cria "Chiquilin") (Explorador "Astutobias" ["Parque Yellowstone", "Bosque Verdoso"] 
                                            (Cria "Enanin") (Explorador "Astutomas" ["Bosque Verdoso", "Rio Azulado"]
                                                            (Cria "Petisin") (Cria "Chiquirrin"))) 
                                       (Cria "Chiquitin"))

lobosLocos :: Lobo
lobosLocos = (Cazador "Colmillonatan" ["Despistadeo", "Perezosandra", "Papanatalia"]
                (Cria "Chiquilin") 
                (Explorador "Astutobias" ["Parque Yellowstone", "Rio Mojado"] 
                    (Cria "Enanin") 
                    (Explorador "Astutomas" ["Bosque Verdoso", "Rio Azulado"]
                        (Cria "Petisin") 
                        (Cria "Chiquirrin")
                    )
                ) 
                (Cria "Chiquitin")
             )     


-- EJERCICIO 4.2:

buenaCaza :: Manada -> Bool
-- PROPÓSITO: Dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza = undefined


-- EJERCICIO 4.3:

elAlfa :: Manada -> (Nombre, Int)
-- PROPÓSITO: Dada una manada, devuelve el nombre del lobo con más presas cazadas, junto con su cantidad de presas. 
-- NOTA: Se considera que los exploradores y crías tienen cero presas cazadas, y que po drían formar parte del resultado 
--       si es que no existen cazadores con más de cero presas.
elAlfa = undefined


-- EJERCICIO 4.4:

losQueExploraron :: Territorio -> Manada -> [Nombre]
-- PROPÓSITO: Dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio.
losQueExploraron = undefined


-- EJERCICIO 4.5:

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
-- PROPÓSITO: Dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es
--            la lista de los nombres de los exploradores que exploraron dicho territorio. Los territorios no deb en repetirse.
exploradoresPorTerritorio = undefined

-- EJERCICIO 4.6:

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
-- PROPÓSITO: Dado el nombre de un lobo y una manada, indica el nombre de todos los cazadores que tienen como subordinado al
--            lobo dado (puede ser un subordinado directo, o el subordinado de un subordinado).
-- PRECONDICIÓN: Hay un lobo con dicho nombre y es único.
cazadoresSuperioresDe = undefined