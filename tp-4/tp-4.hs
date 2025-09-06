-- EJERCICIO 1: Pizzas.

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

pizza0 :: Pizza
pizza0 = Capa (Aceitunas 9) (Capa Jamon (Capa Queso (Capa Salsa Prepizza)))


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



-- EJERCICIO 2.1:



-- EJERCICIO 2.2:



-- EJERCICIO 2.3:



-- EJERCICIO 2.4:



-- EJERCICIO 2.5:



-- EJERCICIO 2.6:




-- ####################################################################################################################### --

-- EJERCICIO 3: Nave Espacial.



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