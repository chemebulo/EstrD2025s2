-- EJERCICIO 1: Invariantes de Representación del TAD Nave.

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
    deriving Show

{- INV. REP.:
    * Siendo N mss mnt mht:
        * Cada clave SectorId en MSS debe ser igual al SectorId del Sector que tiene asignado como valor.
        * El SectorId de cada valor Sector en MSS debe ser igual al SectorId que tiene como clave.
        * El nombre de cada valor Tripulante en MNT debe ser igual al Nombre que tiene como clave.
        * Cada clave Nombre en MNT debe ser igual al Nombre del Tripulante que tiene asignado como valor.
        * Los sectores de cada valor Tripulante que tienen Nombre como clave en MNT, deben existir como valor
          en MSS, y además tener a dicho Tripulante.
        * Los tripulantes de cada valor Sector que tienen un SectorId como clave en MSS, deben existir en MNT como 
          valor y además tener a dicho Sector.
        * Cada Tripulante que existe en MHT debe existir como valor en MNT.
        * Cada valor Tripulante que tiene Nombre como clave en MNT debe existir como elemento en MHT.
-}

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2: Implementación del TAD Nave.

-- EJERCICIO 2.A:

construir :: [SectorId] -> Nave
-- PROPÓSITO: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de Sectores en la lista, se utiliza la función "construirS" de costo "S log S" en la lista dada. Además, se
    -- utilizan las funciones "emptyM" y "emptyH" de costo constante. Es por eso que el costo total de la función en el peor caso es
    -- "S log S + 1 + 1", que simplificado es "S log S".
construir ss = N (construirS ss) emptyM emptyH


construirS :: [SectorId] -> Map SectorId Sector
-- PROPÓSITO: Construye un Map con sectores vacíos, en base a una lista de identificadores de sectores.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de Sectores en la lista, por cada S se utiliza la función "assocM" de costo "log K", aunque en este contexto
    -- se puede entender como "log S" porque recorre claves de tipo SectorId. Además se utiliza la función "crearS" de costo constante.
    -- Es por eso que el costo total de la función en el peor caso es "S * (log S + 1)", que simplificado es "S log S".
construirS []     = emptyM
construirS (s:ss) = assocM s (crearS s) (construirS ss)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.B:

ingresarT :: Nombre -> Rango -> Nave -> Nave
-- PROPÓSITO: Incorpora un tripulante a la nave, sin asignarle un sector.
-- COSTO: O(log N + log T).
    -- Siendo T la cantidad de Tripulantes y N la cantidad de Nombres en la nave; se utilizan las funcioens "assocM" de costo "log K",
    -- que en este contexto se puede entender como "log N" porque recorre claves de tipo Nombre. Además se utiliza la función "insertH"
    -- de costo "log M", que en este contexto se puede entender como "log T" porque recorre elementos de tipo Tripulante. También se
    -- utiliza la función "crearT" de costo constante. Es por eso que el costo total de la función en el peor caso es "1 + log N + log T",
    -- que simplificado es "log N + log T".  
ingresarT n r (N mss mnt mht) = let nTrip = crearT n r;
                                    nMNT = assocM n nTrip mnt;
                                    nMHT = insertH nTrip mht 
                                 in N mss nMNT nMHT

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.C:

sectoresAsignados :: Nombre -> Nave -> Set SectorId
-- PROPÓSITO: Devuelve los sectores asignados a un tripulante.
-- PRECONDICIÓN: Existe un tripulante con dicho nombre. 
-- COSTO: O(log N).
    -- Siendo N la cantidad de Nombres en la nave, se utiliza la función "lookupM" de costo "log K", que en este contexto se puede entender
    -- como "log N" porque recorre claves de tipo Nombre. Además se utiliza la función "sectoresT" de costo constante. Es por eso que el
    -- costo total de la función en el peor caso es "1 + log N", que simplificado es "log N".
sectoresAsignados n (N _ mnt _) = case lookupM n mnt of
                                    Just t  -> sectoresT t
                                    Nothing -> error "No existe ningún tripulante con dicho nombre." 

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.D:

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
-- PROPÓSITO: Dado un Sector, devuelve los Tripulantes y los Componententes asignados a ese Sector.
-- PRECONDICIÓN: Existe un Sector con dicho Id.
-- COSTO: O(log S).
    -- Siendo S la cantidad de Sectores en la nave, se utiliza la función "lookupM" de costo "log K", que en este contexto se puede entender
    -- como "log S" porque recorre claves de tipo SectorId. Además se utiliza la función "tripulantesS" y "componentesS", ambas de 
    -- costo constante. Es por eso que el costo total de la función en el peor caso es "1 + 1 + log S", que simplificado es "log S". 
datosDeSector sid (N mss _ _) = case lookupM sid mss of
                                  Just s  -> (tripulantesS s, componentesS s)
                                  Nothing -> error "No existe un Sector con dicho Id."

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.E:

tripulantesN :: Nave -> [Tripulante]
-- PROPÓSITO: Devuelve una lista de Tripulantes ordenada por rango, de mayor a menor.
-- COSTO: O(T log T).
    -- Siendo T la cantidad de Tripulantes en la nave, se utiliza la función "tripulantesMHT" de costo "T log T" en MHT (la heap de la nave).
    -- Es por eso que el costo total de la función en el peor caso es "T log T".
tripulantesN (N mss mnt mht) = tripulantesMHT mht 

tripulantesMHT :: MaxHeap Tripulante -> [Tripulante]
-- PROPÓSITO: Devuelve una lista con los Tripulantes de la MaxHeap Tripulante dada.
-- COSTO: O(T log T).
    -- Siendo T la cantidad de Tripulantes en la heap, por cada Tripulante se utiliza la función "isEmptyH" y "maxH", ambas de costo
    -- constante. Además se utiliza la función "deleteMaxH" de costo "log M", que en este contexto se puede entender como "log T" porque
    -- se recorren elementos de tipo Tripulantes. Es por eso que el costo total de la función en el peor caso es "1 + 1 + T log T", que
    -- es simplificado como "T log T". 
tripulantesMHT mht = if (isEmptyH mht)
                        then []
                        else maxH mht : tripulantesMHT (deleteMaxH mht)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.F:

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- PROPÓSITO: Asigna una lista de Componentes a un Sector de la Nave.
-- COSTO: O(C + log S).
    -- Siendo S la cantidad de Sectores en la nave, se utiliza la función "lookupM" y "assocM" ambas de costo "log K", que en este contexto
    -- se pueden entender como "log S" porque se recorren claves de tipo SectorId. Además se utiliza la función "agregarComponentesASector"
    -- de costo "C" (lineal) en la lista de Componentes dada. Es por eso que el costo total de la función en el peor caso es "C + log S".  
agregarASector cs sid (N mss mnt mht) = case lookupM sid mss of
                                          Just s  -> let nSector = agregarComponentesASector cs s;
                                                         nMSS = assocM sid nSector mss
                                                      in N nMSS mnt mht
                                          Nothing -> error "No existe un Sector con dicho Id."

agregarComponentesASector :: [Componente] -> Sector -> Sector
-- PROPÓSITO: Agrega los Componentes al Sector dado.
-- COSTO: O(C).
    -- Siendo C la cantidad de Componentes en la lista, por cada Componente se utiliza la función "agregarC" de costo constante. Es por eso
    -- que el costo total de la función en el peor caso es "C" (lineal). 
agregarComponentesASector []     s = s
agregarComponentesASector (c:cs) s = agregarC c (agregarComponentesASector cs s)  

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.G:

asignarASector :: Nombre -> SectorId -> Nave -> Nave
-- PROPÓSITO: Asigna un Sector a un Tripulante.
-- PRECONDICIÓN: El Tripulante y el Sector existen.
-- NOTA: No importa si el Tripulante ya tiene asignado dicho Sector.
-- COSTO: O(log S + log N + T log T).
    -- Siendo S la cantidad de Sectores, N la cantidad de Nombres y T la cantidad de Tripulantes en la nave; se utiliza la función 
    -- "lookupM" y "assocM", ambas de costo "log K", aunque en este contexto es de "log S" y "log N", ya que una recorre claves de tipo 
    -- SectorId y la otra de tipo Nombre. Por último, se utiliza la función "agregarT" de costo "log T", "asignarS" de costo "log S", y
    -- "reemplazarT" de costo "T log T". Es por eso que el costo total de la función en el peor caso es "log S + log N + T log T".
asignarASector n sid (N mss mnt mht) = case lookupM sid mss of
                                         Nothing -> error "No existe un Sector con dicho Id."
                                         Just s  -> case lookupM n mnt of
                                                      Nothing -> error "No existe un Tripulante con dicho Nombre."
                                                      Just t  -> let nSector = agregarT n s;
                                                                     nTrip = asignarS sid t;
                                                                     nMSS = assocM sid nSector mss
                                                                     nMNT = assocM n nTrip mnt;
                                                                     nMHT = reemplazarT nTrip mht
                                                                  in N nMSS nMNT nMHT

reemplazarT :: Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante
-- PROPÓSITO: Reemplaza el Tripulante existente en la MaxHeap Tripulante dada por el Tripulante dado. En el caso que no exista, lo agrega
--            a la MaxHeap.
-- COSTO: O(T log T).
    -- Siendo T la cantidad de Tripulantes en la heap, por cada T se utiliza la función "insertH" y "deleteMaxH", ambas de costo "log M",
    -- aunque en este contexto son de costo "log T", ya que recorren elementos de tipo Tripulante. Además se utiliza la función "nombre",
    -- "isEmptyH" y "maxH", todas de costo constante. Es eso que el costo total de la función en el peor caso es "1 + 1 + 1 + T log T", 
    -- que simplificado es "T log T".
reemplazarT t mht = if isEmptyH mht
                       then []
                       else if (nombre (maxH mht)) == (nombre t) 
                               then insertH t (deleteMaxH mht) 
                               else insertH (maxH mht) (reemplazarT t (deleteMaxH mht))

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3: Implementación desde el lado del Usuario:

-- EJERCICIO 3.A:

sectores :: Nave -> Set SectorId
-- PROPÓSITO: Devuelve todos los sectores no vacíos (con tripulantes asignados).
-- COSTO: O(T * (S log S + log T)).
    -- Siendo T la cantidad de Tripulantes y S la cantidad de Sectores en la nave, se utiliza la función "tripulantesN" de costo "T log T",
    -- además de la función "sectoresTS" de costo "T * (S log S)". Es por eso que el costo total de la función en el peor caso es
    -- "T * (S log S) + T log T", que simplificado es "T * (S log S + log T)".
sectores n = sectoresTS (tripulantesN n)

sectoresTS :: [Tripulante] -> Set SectorId
-- PROPÓSITO: Devuelve todos los sectores no vacíos (con tripulantes asignados).
-- COSTO: O(T * S log S).
    -- Siendo T la cantidad de Tripulantes y S la cantidad de Sectores en el conjunto; por cada T se utiliza la función "sectoresT" de costo
    -- constante, y la función "unionS" de costo "S log S". Es por eso que el costo de la función en el peor caso es "T * S log S".
sectoresTS []     = emptyS
sectoresTS (t:ts) = unionS (sectoresT t) (sectoresTS ts)

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.B:

sinSectoresAsignados :: Nave -> [Tripulante]
-- PROPÓSITO: Devuelve los tripulantes que no poseen sectores asignados.
-- COSTO: O(T log T).
    -- Siendo T la cantidad de Tripulantes en la nave, se utiliza la función "sinSectoresAsignadosTS" de costo "T" (lineal), y también la 
    -- función "tripulantesN" de costo "T log T". Es por eso que el costo total de la función en el peor caso es "T + T log T", que se
    -- simplifica como "T log T". 
sinSectoresAsignados n = sinSectoresAsignadosTS (tripulantesN n)

sinSectoresAsignadosTS :: [Tripulante] -> [Tripulante]
-- PROPÓSITO: Devuelve los tripulantes que no poseen sectores asignados.
-- COSTO: O(T).
    -- Siendo T la cantidad de Tripulantes en la nave, por cada Tripulante se utiliza la función "sizeS" y "sectoresT", ambas de costo
    -- constante. Además se utiliza el constructor cons de costo constante. Es por eso que el costo total de la función en el peor caso
    -- es "T" (lineal).
sinSectoresAsignadosTS []     = []
sinSectoresAsignadosTS (t:ts) = if sizeS (sectoresT t) == 0
                                   then t : sinSectoresAsignadosTS ts
                                   else sinSectoresAsignadosTS ts

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.C:

barriles :: Nave -> [Barril]
-- PROPÓSITO: Devuelve todos los barriles de los sectores asignados de la nave.
-- COSTO: O(T * (S log S + log T) * S^2).
    -- Siendo S la cantidad de Sectores en la Nave y C la cantidad de Componentes de cada Sector; se utiliza la función "barrilesCS" de 
    -- costo "C" (lineal), la función "componentesSID" de costo "S * (log S + S)", la función "setToList" de costo "N", que en este contexto
    -- se puede entender como "S" (lineal) porque recorre elementos de tipo SectorId. También se utiliza la función "sectores" de costo
    -- "T * (S log S + log T)". Es por eso que el costo total de la función en el peor caso es "T * (S log S + log T) + C + S * (log S + S)",
    -- que simplificado es "T * (S log S + log T) * S^2".
barriles n = barrilesCS (componentesSID n (setToList (sectores n)))

componentesSID :: Nave -> [SectorId] -> [Componente]
-- PROPÓSITO: Devuelve todos los componentes de los sectores asignados de la nave.
-- COSTO: O(S * (log S + S)).
    -- Siendo S la cantidad de Sectores en la lista, por cada Sector se utiliza la función "datosDeSector" de costo "log S", y la operación
    -- "++" de costo "S" (lineal). Es por eso que el costo total de la función en el peor caso es "S * (log S + S)".
componentesSID n []     = []
componentesSID n (s:ss) = snd (datosDeSector s n) ++ componentesSID n ss

barrilesCS :: [Componente] -> [Barril]
-- PROPÓSITO: Devuelve todos los barriles de los componentes dados.
-- COSTO: O(C).
    -- Siendo C la cantidad de Componentes en la lista, por cada Componente se utiliza la operación "++" de costo "C" lineal. Es por eso
    -- que el costo total de la función en el peor caso es es "C" (lineal).
barrilesCS []     = []
barrilesCS (c:cs) = case c of
                      Almacen b -> b ++ barrilesCS cs
                      _         -> barrilesCS cs

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3.D:

-- Una posible implementación del TAD Sector podría ser: 

data Sector = S SectorId [Componente] Set Nombre
    deriving Show