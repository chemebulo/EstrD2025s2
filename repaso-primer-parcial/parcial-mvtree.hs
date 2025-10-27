-- ############################################################################################################################# --
--                                          PUNTO 1: Funciones como usuario del TAD MVTree.
-- ############################################################################################################################# --

esMasVendido :: Producto -> CaminoJ -> Int -> MVTree -> Bool
-- PROPÓSITO: Dado un producto, un camino jerárquico, un número n y una jerarquía de categorías, indica si el producto está entre 
--            los n más vendidos de alguna de las categorías del camino. Se supone que el camino es válido respecto de la jerarquía
--            dada. Si esto no sucede debe fallarse con el error adecuado. 
-- COSTO: O(J * (J log SC + P log P) + J^2).
-- J^2                      # por inits.
-- J * (J log SC + P log P) # por esMasVendido'. Siendo la lista de la misma longitud que el camino provisto.
esMasVendido p cs n t = esMasVendido' p (inits cs) n t 

esMasVendido' :: Producto -> [CaminoJ] -> Int -> MVTree -> Bool
-- PROPÓSITO: Indica si el producto dado se encuentra entre los n más vendidos de algunas de las categorías de los diferentes caminos
--            jerárquicos. 
-- COSTO: O(N * (J log SC + P log P)).
-- N                  # Siendo N la longitud de la lista por recursion sobre la misma.
-- J log SC + P log P # Por masVendidosEn.
-- P                  # Siendo P por elems.
esMasVendido' p []       n t = False
esMasVendido' p (cs:css) n t = elems p (masVendidosEn n cs t) || esMasVendido' p css n t

-- FUNCIÓN AUXILIAR PROVISTA EN EL ENUNCIADO:
inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : preprendEach x (inits xs)

preprendEach :: a -> [[a]] -> [[a]]
preprendEach _ []       = []
preprendEach x (ys:yss) = (x:ys) : preprendEach x yss


-- ############################################################################################################################# --
--                                       PUNTO 2: Invariantes de representación del TAD MVTree.
-- ############################################################################################################################# --

type Categoria = String
type CaminoJ   = [Categoria]
type Producto  = String

data MVTree = MVT (Heap (Int, Producto)) (Map Categoria MVTree)
      deriving Show

{- INVARIANTES DE REPRESENTACIÓN:
    * No existen en una Heap dos elementos con la misma segunda componente.
    * Todos los elementos de cualquier Heap tienen su primer componente positiva.
    * Todos los elementos de la Heap se encuentran en los nodos ancestros con la misma cantidad o mayor.
-}

-- ############################################################################################################################# --
--                                      PUNTO 3: Implementación funciones de la interfaz de MVTree.
-- ############################################################################################################################# --


emptyMVTree :: MVTree
-- PROPÓSITO: Representa a una jerarquía de categorías sin categorías.
-- COSTO: O(1).
-- 1 # Por emptyM y emptyH.
emptyMVTree = MVT emptyH emptyM

-- ############################################################################################################################# --

registrarCategoria :: CaminoJ -> MVTree -> MVTree
-- PROPÓSITO: Dado un camino jerárquico y una jerarquía de categorías, describe la jerarquía resultante de agregar categorías a la
--            jerarquía dada de forma tal que todas las mencionadas en el camino existan como subcategorías de la anterior (si ya
--            existían, pueden agregar las subcategorías necesarias, y si no, se crean sin productos). 
--            Por ejemplo, registarCategoria ["Computacion", "Accesorios", "Pads"], se agregaría "Accesorios" como subcategoría
--            de "Computación" y luego "Pads" como subcategoría de la nueva "Accesorios".
-- COSTO: O(J * log SC).
-- J       # Por la recursión sobre el camino.
-- log SC  # Por lookupM y assocM.
registrarCategoria []     t            = t
registrarCategoria (c:cs) (MVTree h m) = case lookupM c m of
                                           Nothing -> MVT h (assocM c (registrarCategoria cs (MVT emptyH emptyM)) m) 
                                           Just t  -> MVT h (assocM c (registrarCategoria cs t) m)  

-- ############################################################################################################################# --

subcategoriasDe :: CaminoJ -> MVTree -> [Categoria]
-- PROPÓSITO: Dado un camino jerárquico y una jerarquía de categorías, describe la lista de todas las subcategorías de la última 
--            categoría del camino. Se supone que el camino es válido respecto de la jerarquía dada. Si esto no sucede debe fallarse
--            con un error adecuado.
-- COSTO: O(SC + J * log SC).
-- J       # Por la recursión sobre el camino.
-- log SC  # Por lookupM.
-- SC      # Por domM.
subcategoriasDe []     (MVTree h m) = domM m
subcategoriasDe (c:cs) (MVTree h m) = case lookupM c m of
                                        Nothing -> error "El camino no es válido."
                                        Just t  -> subcategoriasDe cs t

-- ############################################################################################################################# --

registrarVenta :: Producto -> CaminoJ -> MVTree -> MVTree
-- PROPÓSITO: Dado un producto, un camino jerárquico y una jerarquía de categorías, describe la jerarquía que resulta de agregar una
--            venta del producto en cada una de las categorías del camino. Se supone que el camino es válido respecto de la jerarquía
--            dada. Si esto no sucede debe fallarse con un error adecuado.
-- COSTO: O(J * (log SC + P log P)).
-- J       # Por la recursión sobre el camino.
-- log SC  # Por lookupM y assocM.
-- P log P # Por registrarVentaH.
registrarVenta p []     (MVT h m) = MVT (registrarVentaH p h) m
registrarVenta p (c:cs) (MVT h m) = case lookupM c m of
                                      Nothing -> error "El camino no es válido."
                                      Just t  -> MVT (registrarVentaH p h) (assocM c (registrarVenta p cs t) m)

registrarVentaH :: Producto -> Heap (Int, Producto) -> Heap (Int, Producto)
-- PROPÓSITO: Dado un producto y una Heap, registra la venta del producto en la misma.
-- COSTO: O(P log P).
-- P     # Siendo P la cantidad de elementos de la Heap por recursión sobre la misma.
-- log P # Por insertH y deleteMaxH.
registrarVentaH p h = if isEmptyH h
                         then insertH (1, p) h
                         else let (i, p') = findMaxH h
                               in if p == p'
                                     then insertH (i+1, p) (deleteMaxH h)
                                     else insertH (i, p') (registrarVentaH p (deleteMaxH h))

-- ############################################################################################################################# --

masVendidosEn :: Int -> CaminoJ -> MVTree -> [Producto]
-- PROPÓSITO: Dado un número n, un camino jerárquico y una jerarquía de categorías, describe la lista de los n productos más vendidos
--            en la categoría final del camino. Se supone que el camino es válido respecto de la jerarquía dada. Si esto no sucede
--            debe fallarse con un error adecuado.
-- PRECONDICIÓN: N no puede ser negativo.
-- COSTO: O(log SC + P log P). 
-- log SC  # Por lookupM.
-- P log P # Por takeH
masVendidosEn n []     (MVT h m) = if n < 0
                                      then []
                                      else takeH n h
masVendidosEn n (c:cs) (MVT h m) = case lookupM c m of
                                      Nothing -> error "El camino no es válido."
                                      Just t  -> masVendidosEn n cs t

takeH :: Int -> Heap (Int, Producto) -> [Producto]
-- PROPÓSITO: Dado un número y una Heap, devuelve la lista con los n productos más vendidos en la misma.
-- COSTO: O(P log P).
-- P     # Siendo P la cantidad de elementos de la heap por recursión sobre la misma.
-- log P # Por deleteMaxH.
takeH 0 h = [] 
takeH n h = if isEmptyH h
               then []
               else let (i, p) = findMaxH h
                     in p : takeH (n-1) (deleteMaxH h)

-- ############################################################################################################################# --