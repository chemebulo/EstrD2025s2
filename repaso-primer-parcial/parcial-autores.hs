-- EJERCICIO 1.A:

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
-- PROPÓSITO: Dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas programaron juntas.
-- PRECONDICIÓN: Ambas personas deben existir en el organizador.
-- COSTO: O(C log C + log P).
    -- Siendo P la cantidad de personas y C la cantidad de programas en el organizador, se utilizan las operaciones "programasDe" de
    -- costo "log P", "setToList" de costo lineal en "C", y "programasEnComun'" de costo "C log C". Es por eso que el costo total de la
    -- función en el peor caso es "C log C + log P".
programasEnComun p1 p2 org = programasEnComun' (setToList (programasDe org p1)) (programasDe org p2)

programasEnComun' :: [Checksum] -> Set Checksum -> Set Checksum
-- PROPÓSITO: Dada una lista de programas y un Set de programas, denota el conjunto de aquellos programas que aparecen en la lista y
--            en el conjunto dado.
-- COSTO: O(C log C).
    -- Siendo C la cantidad de programas en el organizador, por cada programa de la lista se realiza la operación "belongs" de costo
    -- "log C" en el Set dado. Además se utiliza la operación "addS" de costo "log C". Es por eso que el costo total de la función en
    -- el peor caso es "C log C".
programasEnComun []     sc = 
programasEnComun (c:cs) sc = if belongs c cs2
                                then addS c (programasEnComun' cs cs2)
                                else programasEnComun' cs cs2

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 1.B:

esUnGranHacker :: Organizador -> Persona -> Bool
-- PROPÓSITO: Denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
-- COSTO: O(C * (log C + log P)).
    -- Siendo P la cantidad de personas, C la cantidad de programas del organizador, "todosLosProgramas" de costo lineal en "C", además
    -- de la función "esAutorDeTodos" de costo "C * (log C + log P)". Es por eso que el costo total de la función en el peor caso es 
    -- "C * (log C + log P)".
esUnGranHacker org p = esAutorDeTodos (todosLosProgramas org) p org

esAutorDeTodos :: [Checksum] -> Persona -> Organizador -> Bool
-- PROPÓSITO: Indica si la persona es autora de todos los programas de la lista.
-- COSTO: O(C * (log C + log P)).
    -- Siendo P la cantidad de personas, C la cantidad de programas del organizador, por cada programa se utiliza la operación "autoresDe"
    -- de costo "log C", además de la operación "belongs" de costo "log P". Es por eso que el costo total de la función en el peor caso
    -- es "C * (log C + log P)".
esAutorDeTodos []     p org = True
esAutorDeTodos (c:cs) p org = belongs p (autoresDe org c) && esAutorDeTodos cs p org 

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.A:

data Organizador = MKO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
    deriving Show

{- INV. REP.:
    * Siendo MK0 mcsp mpsc:
        * Cada Checksum de cada persona en MPSC debe existir en MCSP como clave, y tener como valor a esa persona.
        * Cada Persona de cada Checksum en MCSP debe existir en MPSC como clave, y tener como valor a ese Checksum.
-}

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2.B:

nuevo :: Organizador
-- PROPÓSITO: Un organizador vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "emptyM" de costo constante.
nuevo = MK0 emptyM emptyM

----------------------------------------------------------------------------------------------------------------------------------------

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
-- PROPÓSITO: Agrega al organizaodr un programa con el Checksum indicado; el conjunto es el conjunto de las personas autores
--            de dicho programa.
-- PRECONDICIÓN: El identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
--               no está vacío.
-- COSTO: O(P * (log P + log C)).
    -- Siendo P la cantidad de Personas, C la cantidad de Checksum en el organizador, se utiliza la operación "assocM" de costo "log C",
    -- la función "añadirProgramaAAutores" de costo "P * (log P + log C)", y "setToList" de costo "log P". Es por eso que el costo total
    -- de la función en el peor caso es "P * (log P + log C)".
agregarPrograma (MK0 mcsp mpsc) c sp = let nMCSP = assocM c sp mcsp;              
                                           nMPSC = añadirProgramaAAutores c (setToList sp) mpsc
                                        in MK0 nMCSP nMPSC

añadirProgramaAAutores :: Checksum -> [Persona] -> Map Persona (Set Checksum) -> Map Persona (Set Checksum) 
-- PROPÓSITO: Añade el programa a todos los autores de la lista en el Map dado, resultando en el Map dado con sus modificaciones.
-- COSTO: O(P * (log P + log C)).
    -- Siendo P la cantidad de personas, C la cantidad de Checksum en el organizador, por cada persona se realiza la operación "lookupM"
    -- de costo "log P" en el map y en caso de que exista, se utiliza la operación "addS" de costo "log C" en el Set de Checksum. Es por
    -- eso que el costo total de la función en el peor caso es "P * (log P + log C)".
añadirProgramaAAutores c []     mpsc = mpsc
añadirProgramaAAutores c (p:ps) mpsc = case lookupM p mpsc of
                                         Just sc -> assocM p (addS c sc) (añadirProgramaAAutores c ps mpsc)
                                         Nothing -> assocM p (addS c emptyS) (añadirProgramaAAutores c ps mpsc)

----------------------------------------------------------------------------------------------------------------------------------------

todosLosProgramas :: Organizador -> [Checksum]
-- PROPÓSITO: Denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- COSTO: O(C).
    -- Siendo C la cantidad de códigos en el organizador, se realiza la operación "domM" de lineal en "C". Es por eso que el
    -- costo total de la función es "C" (lineal).
todosLosProgramas (MK0 mcsp _) = domM mcsp

----------------------------------------------------------------------------------------------------------------------------------------

autoresDe :: Organizador -> Checksum -> Set Persona
-- PROPÓSITO: Denota el conjunto de autores que aparecen en un programa determinado.
-- PRECONDICIÓN: El Checksum debe corresponder a un programa del organizador.
-- COSTO: O(log C).
    -- Siendo C la cantidad total de Checksum del organizador, se utiliza la operación "lookupM" de costo "log C". Es por eso que
    -- el costo total de la función es "log C".
autoresDe (MK0 mcsp _) c = case lookupM c mcsp of
                             Just sp  -> sp
                             Nothingt -> error "El Checksum dado no corresponde a un programa del organizador."

----------------------------------------------------------------------------------------------------------------------------------------

programasDe :: Organizador -> Persona -> Set Checksum
-- PROPÓSITO: Denota el conjunto de programas en los que participó una determinada persona.
-- PRECONDICIÓN: La persona debe existir en el organizador.
-- COSTO: O(log P).
    -- Siendo P la cantidad total de Personas del organizador, se utiliza la operación "lookupM" de costo "log P". Es por eso que
    -- el costo total de la función es "log P".
programasDe (MK0 _ mpsc) p = case lookupM p mpsc of
                               Just sc -> sc
                               Nothing -> error "La Persona dada no existe en el organizador." 

----------------------------------------------------------------------------------------------------------------------------------------

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
-- PROPÓSITO: Dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- PRECONDICIÓN: Las personas deben ser distintas.
-- COSTO: O(log P + C log C).
    -- Siendo P la cantidad de personas, C la cantidad de Checksum en el organizador, se utilizan las operaciones "lookupM" de costo
    -- "log P", además "intersection" de costo "C log C", y "isEmpty" de costo constante. Es por eso que el costo total de la función
    -- en el peor caso es "log P + C log C".
programaronJuntas (MK0 _ mpsc) p1 p2 = case (lookupM p1 mpsc, lookupM p2 mpsc) of
                                         (Just sc1, Just sc2) -> not isEmpty (interserction sc1 sc2 )
                                         _                    -> error "Algunas (o las dos) de las personas no existen en el organizador." 

----------------------------------------------------------------------------------------------------------------------------------------

nroProgramasDePersona :: Organizador -> Persona -> Int
-- PROPÓSITO: Dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
-- PRECONDICIÓN: La persona debe existir en el organizador.
-- COSTO: O(log P).
    -- Siendo P la cantidad total de Personas del organizador, se utiliza la operación "lookupM" de costo "log P" y "sizeS" de costo
    -- constante. Es por eso que el costo total de la función en el peor caso es "log P".
nroProgramasDePersona (MK0 _ mpsc) p = case lookupM p mpsc of
                                         Just sc -> sizeS sc
                                         Nothing -> error "La Persona no existe en el organizador." 

----------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3:

-- Para que el Ejercicio 3 sea posible, requieron los siguientes cambios:

data Organizador = MKO (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) (Maybe Checksum)
    deriving Show

{- INV. REP.:
    * Siendo MK0 mcsp mpsc mc:
        * Cada Checksum de cada persona en MPSC debe existir en MCSP como clave, y tener como valor a esa persona.
        * Cada Persona de cada Checksum en MCSP debe existir en MPSC como clave, y tener como valor a ese Checksum.
        * Si MCSP se encuentra vacío, MC es Nothing. En caso contrario MC es el Checksum con más autores de MCSP (el Checksum con la mayor
          longitud del Set de Persona).
-}

----------------------------------------------------------------------------------------------------------------------------------------

elMayorPrograma :: Organizador -> Maybe Checksum
-- PROPÓSITO: Recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota Nothing 
--            si no puede devolver un programa.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se abre la estructura y se devuelve uno de sus campos.
elMayorPrograma (MK0 mcsp mpsc mc) = mc

----------------------------------------------------------------------------------------------------------------------------------------

nuevo :: Organizador
-- PROPÓSITO: Un organizador vacío.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "emptyM" de costo constante. 
nuevo = MK0 emptyM emptyM Nothing

----------------------------------------------------------------------------------------------------------------------------------------

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
-- PROPÓSITO: Agrega al organizaodr un programa con el Checksum indicado; el conjunto es el conjunto de las personas autores
--            de dicho programa.
-- PRECONDICIÓN: El identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
--               no está vacío.
-- COSTO: O(P * (log P + log C)).
    -- Siendo P la cantidad de Personas, C la cantidad de Checksum en el organizador, se utiliza la operación "assocM" de costo "log C",
    -- la función "añadirProgramaAAutores" de costo "P * (log P + log C)", y "setToList" de costo "log P". Es por eso que el costo total
    -- de la función en el peor caso es "P * (log P + log C)".
agregarPrograma (MK0 mcsp mpsc mc) c sp = let nMCSP = assocM c sp mcsp;              
                                              nMPSC = añadirProgramaAAutores c (setToList sp) mpsc
                                           in case mc of
                                                Just c' -> if sizeS (fromJust (lookupM c' mcsp)) > sizeS sp
                                                              then (MK0 nMCSP nMPSC c')
                                                              else (MK0 nMCSP nMPSC c)
                                                _       -> (MK0 nMCSP nMPSC c)