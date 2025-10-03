-- EJERCICIO 1: Implementación de funciones como Usuario.

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 1.A: 

tripulantes :: Nave -> Set Tripulante
-- PROPÓSITO: Denota los Tripulantes de la Nave.
-- COSTO: O(S * (T log T + log S)).
    -- Siendo T la cantidad de Tripulantes y S la cantidad de Sectores en la Nave, se utiliza la función "tripulantesDeSectores" de costo
    -- "S * (T log T + log S)" en el peor caso en la cantidad de Sectores. Además se utiliza la función "sectores" de costo "S" (lineal)
    -- en el peor caso. Es por eso que el costo total de la función en el peor caso es "S * (T log T + log S) + S", que simplificado es
    -- "S * (T log T + log S")".
tripulantes n = tripulantesDeSectores (sectores n) n

tripulantesDeSectores :: [Sector] -> Nave -> Set Tripulante
-- PROPÓSITO: Describe el conjunto de los Tripulantes que pertenecen a los sectores de la lista dada.
-- PRECONDICIONES:
--  * Los Sectores dados deben existir en la Nave dada.
-- COSTO: O(S * (T log T + log S)).
    -- Siendo S la cantidad de Sectores y T la cantidad de Tripulantes en la Nave, por cada Sector se utilizan las funciones "union" de
    -- costo "N log N", aunque en este contexto se puede entender como "T log T" ya que recorre elementos de tipo Tripulante. Además, 
    -- se utiliza "tripulantesDe" de costo "log S". Es por eso que el costo total de la función en el peor caso es "S * (T log T + log S)".
tripulantesDeSectores []     n = emptyS
tripulantesDeSectores (s:ss) n = union (tripulantesDe s n) (tripulantesDeSectores ss n)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 1.B:

bajaDeTripulante :: Tripulante -> Nave -> Nave
-- PROPÓSITO: Elimina al Tripulante dado de la Nave.
-- COSTO: O(S * (T * (log S + log T) + log S)).
    -- Siendo S la cantidad de Sectores y T la cantidad de Tripulantes en la Nave, se utiliza la función "armarNaveConSectoresSinTripulante"
    -- de costo "S * (T * (log S + log T) + log S)". Es por eso que el costo total de la función en el peor caso es "S * (T * (log S +
    -- log T) + log S)". 
bajaDeTripulante t n = armarNaveConSectoresSinTripulante (sectores n) t n

armarNaveConSectoresSinTripulante :: [Sector] -> Tripulante -> Nave -> Nave
-- PROPÓSITO: Describe una Nave con todos los Sectores dados sin el Tripulante dado.
-- PRECONDICIONES:
--  * Los Sectores dados deben existir en la Nave dada.
-- COSTO: O(S * (T * (log S + log T) + log S))".
    -- Siendo S la cantidad de Sectores en la lista y T la cantidad de Tripulantes en la Nave, por cada Sector se utiliza la función
    -- "armarSectorSinTripulante" de costo "T * (log S + log T)" en el peor caso, además de "set2List" de costo "N" (lineal) aunque en
    -- en este caso puede entenderse como "T" ya que recorre elementos de tipo Tripulante. Por último, se utiliza la función "tripulantesDe"
    -- de costo "log S" y "naveVacia" de costo "S log S". Es por eso que el costo total de la función en el peor caso es "T * (log S + 
    -- log T) + S + log S + S log S", que simplificado es "S * (T * (log S + log T) + log S".
armarNaveConSectoresSinTripulante []	 t n =
armarNaveConSectoresSinTripulante (s:ss) t n = armarSectorSinTripulante s 
								       	                                (set2List (tripulantesDe s n)) 
								       	                                t 
								       	                                (armarNaveConSectoresSinTripulante ss t n)

armarNaveConSectoresSinTripulante :: Sector -> [Tripulante] -> Tripulante -> Nave -> Nave
-- PROPÓSITO: Describe la Nave después de agregar los Tripulante sin el Tripulante dado al Sector dado.
-- PRECONDICIONES:
--  * La lista de Tripulantes no tiene repetidos.
--  * El Sector debe existir en la Nave.
-- COSTO: O(T * (log S + log T)).
    -- Siendo S la cantidad de Sectores en la Nave y T la cantidad de Tripulantes en la lista, por cada Tripulante se utiliza la función
    -- "agregarTripulante" de costo "log S + log T" en el peor caso. Es por eso que el costo total de la función en el peor caso es
    -- "T * (log S + log T)".
armarNaveConSectoresSinTripulante s []     t' n = 
armarNaveConSectoresSinTripulante s (t:ts) t' n = if n /= n'
					   	                             then agregarTripulante t s (armarNaveConSectoresSinTripulante s ts t' n)
						                             else agregarNaveConSectoresSinTripulante s ts t' n

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.A:

data Nave = MKN (Map Sector (Set Tripulante))
		(Heap Tripulante)
		(Sector, Int)
    deriving Show

{-
* INVARIANTES DE REPRESENTACIÓN:
    * Siendo MSST HT SI:
        * Cada elemento Tripulante en "HT" debe existir en el conjunto como valor en alguna clave Sector de "MSST".
        * Cada Tripulante de cada conjunto valor asociado a una clave en "MSST", debe existir como elemento en "HT".
        * El Sector y su longitud en "SI" debe ser igual al Sector que se encuentra como clave en "MSST", además la longitud del
          conjunto que tiene como valor debe coincidir la longitud que se encuentra en "SI".
        * El Sector en "SI" debe existir como clave en "MSST".
-}

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.A:

naveVacia :: [Sector] -> Nave
-- PROPÓSITO: Crea una Nave con todos esos Sectores sin Tripulantes.
-- PRECONDICIONES:
--  * La lista de Sectores no está vacía.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de Sectores de la lista, se utiliza la función "agregarSectores" de costo "S log S" en el peor caso. También
    -- se utiliza la función "emptyH" y "head" ambas de costo constante. Es por eso que el costo total de la función es "S log S + 1 + 1",
    -- que simplificado es "S log S".
naveVacia ss = MKN (agregarSectores ss) emptyH (head ss, 0)

agregarSectores :: [Sector] -> Map Sector (Set Tripulante)
-- PROPÓSITO: Describe el Map con los Sectores dados coo clave y sin Tripulantes.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de Sectores de la lista, por cada Sector se utiliza la función "assocM" de costo "log N" aunque en este
    -- contexto se puede entender como "log S" ya que recorre claves de tipo Sector y "emptyS" de costo constante. Es por eso que el
    -- costo total de la función en el peor caso es "S log S + 1", que simplificado es "S log S".
agregarSectores []     = emptyM
agregarSectores (s:ss) = assocM s (emptyS) (agregarSectores ss)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.B:

tripulantesDe :: Sector -> Nave -> Set Tripulante
-- PROPÓSITO: Obtiene los Tirpulantes de un Sector.
-- PRECONDICIONES:
--  * El Sector dado debe existir en el Map.
-- COSTO: O(log S).
    -- Siendo S la cantidad de Sectores en la Nave, se utiliza la función "lookupM" de costo "log N" aunque en este contexto se puede
    -- entender como "log S" porque recorre claves de tipo Sector. Es por eso que el costo total de la función en el peor caso es "log S".
tripulantesDe s n = case lookupM s n of
                      Just st -> st
                      Nothing -> error "El Sector no existe en la Nave."

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.C:

sectores :: Nave -> [Sector]
-- PROPÓSITO: Denota los Sectores de la Nave.
-- COSTO: O(S).
    -- Siendo S la cantidad de Sectores en la Nave, se utiliza la función "domM" de costo "N" (lineal) aunque en este caso puede entenderse
    -- como "S" porque recorre claves de tipo Sector. Es por eso que el costo total de la función es "S" (lineal).
sectores (MKN msst _ _) = domM msst

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.D:

conMayorRango :: Nave -> Tripulante
-- PROPÓSITO: Denota el Tripulante con mayor rango.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la función "findMinH".
conMayorRango (MKN _ ht _) = findMinH ht

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.E:

conMasTripulante :: Nave -> Sector
-- PROPÓSITO: Denota el Sector de la Nave con más Tripulantes.
-- PRECONDICIONES:
    * La Nave no está vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que se utiliza la función "head".
conMasTripulante (MKN _ _ si) = head si

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.F:

conRango :: Rango -> Nave -> Set Tripulante
-- PROPÓSITO: Denota el conjunto de Tripulantes con dicho rango.
-- COSTO: O(P log P).
    -- Siendo P la cantidad de Tripulantes en la Nave, se utiliza la función "conRangoHT" de costo "P log P". Es por eso que el costo
    -- total de la función es "P log P".
conRango r (MKN _ ht _) = conRangoHT r ht

conRangoHT :: Rango -> Heap Tripulante
-- PROPÓSITO: Describe el conjunto de Tirpulantes con dicho rango.
-- COSTO: O(P log P).
    -- Siendo P la cantidad de Tripulantes en la Heap, por cada Tripulante se utilizan las funciones "deleteMaxH" de costo "log N"
    -- aunque en este contexto se puede entender como "log P" porque recorre elementos de tipo Tripulante. Además "addS" de costo "log P"
    -- (misma explicación que "deleteMaxH"). Es por eso que el costo total de la función en el peor caso es "P log P".
conRangoHT r ht = if isEmptyH ht
		             then []
		             else if rango (findMaxH ht) == r
			                 then addS (findMaxH ht) (conRangoHT r (deleteMaxH ht))
			                 else conRangoHT r (deleteMaxaH ht)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.G:

sectorDe :: Tripulante -> Nave -> Sector
-- PROPÓSITO: Devuelve el Sector en el que se encuentra un Tripulante.
-- PRECONDICIONES:
--  * El Tripulante pertenece a la Nave.
-- COSTO: O(S * (log S + log P)).
    -- Siendo S la cantidad de Sectores y P la cantidad de Tripulantes en la Nave, se utiliza la función de "sectorDeT" de costo 
    -- "S * (log S + log P)" en el peor caos. Además de "domM" de costo "N" (lineal) aunque en este contexto se puede entender como "S"
    -- porque recorre claves de tipo Sector. Es por eso que el costo total de la función en el peor caso es "S * (log S + log P)".
sectorDe t (MKN msst _ _) = sectorDeT t (domM msst) msst

sectorDeT :: Tripulante -> [Sector] -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
-- PROPÓSITO: Devuelve el Sector en el que se encuentra el Tripulante dado.
-- PRECONDICIONES:
--  * El Tripulante pertenece a algún Sector de los Sectores dados.
-- COSTO: O(S * (log S + log P)).
    -- Siendo S la cantidad de Sectores en la lista y P la cantidad de Tripulantes en el Map, por cada Sector se utiliza la función
    -- "lookupM" de costo "log N" aunque en este contexto se puede entender como "log S" porque recorre claves de tipo Sector, y 
    -- "belongs" de costo "log N" aunque en este contexto se puede entender como "log P" porque recorre elementos de tipo Tripulante.
    -- Es por eso que el costo total de la función en el peor caso es "S * (log S + log P)".
sectorDeT t []     m = error "El Tripulante no pertence a ninguno de los Sectores dados."
sectorDeT t (s:ss) m = case lookupM s m of
	                     Just st -> if belongs t st
	       	                           then s
	       	                           else sectorDeT t ss m
	                     Nothing -> sectorDeT t ss m

----------------------------------------------------------------------------------------------------------------------------------------

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
-- PROPÓSITO: Agrega un Tripulante a ese Sector de la Nave.
-- PRECONDICIONES:
--  * El Sector está en la Nave y el Tripulante no.
-- COSTO: O(log S + log P).
    -- Siendo S la cantidad de Sectores y P la cantidad de Tripulantes en la Nave, se utiliza la función "assocM" de costo "log S" y 
    -- "insertH" de costo "log N" aunque en este contexto se puede entender como "log P" porque recorre elementos de tipo Tripulante.
    -- Sumado a esto, se utiliza "addS" de costo "log P" y "sizeS" de costo constante. Es por eso que el osto total de la función en
    -- en el peor caso es "log S + log P".
agregarTripulante t s (MKN msst ht si) = case lookupM s msst of
					                       Nothing -> error "El Sector no existe en la Nave."
					                       Just st -> let nST = addS t st;
						                             	  nMSST = assocM s nST msst;
						                             	  nHT = insertH s ht;
						                               in if (sizeS nST > snd si)
						                                     then let nSI = (s, sizeS nSI)
						                             	           in (MKN nMSST nHT nSI)
						                             	     else (MKN nMSST nHT si)

----------------------------------------------------------------------------------------------------------------------------------------