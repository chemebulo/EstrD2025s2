module Empresa
    (Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, 
              agregarEmpleado, agregarASector, borrarEmpleado)
where 

import Map
import Set

type SectorId = Int
type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)
    deriving Show
{- INV. REP.:
    * Siendo ConsE MSS MCE:
        * Cada valor dentro del Set de valores de cada clave en MSS, si existe, debe existir también como valor de alguna clave en MCE.
        * Cada valor de cada clave en MCE debe estar en las claves de MSS que tenga asignadas.
        * Cada valor de cada clave en MCE debe tener como CUIL el mismo valor de la clave que tiene relacionada.
-}

{- COSTO OPERACIONAL DE CADA FUNCIÓN:

- consEmpresa         O(1)
- buscarPorCUIL       O(log C)
- empleadosDelSector  O(log S + E)
- todosLosCUIL        O(C)
- todosLosSectores    O(S)
- agregarSector       O(log S)
- agregarEmpleado     O(S log S + log E + log C)
- agregarASector      O(log S + log C + log E)
- borrarEmpleado      O(S log S + log C + log E)

-}

{- 
    -------------------------------------------------------------------------------
    |                     INTERFACES DISPONIBLES COMO USUARIO                     |
    |-----------------------------------------------------------------------------|
    |          MAP         |          SET          |           EMPLEADO           |
    |----------------------|-----------------------|------------------------------|
    |  emptyM    O(1)      |  emptyS     O(1)      |  consEmpleado      O(1)      |
    |  assocM    O(log N)  |  addS       O(log N)  |  cuil              O(1)      |
    |  lookupM   O(log N)  |  belongs    O(log N)  |  incorporarSector  O(log S)  |
    |  deleteM   O(log N)  |  sizeS      O(1)      |  sectores          O(S)      |
    |  keys      O(n)      |  removeS    O(log N)  |                              |
    |                      |  unionS     O(N)      |                              |
    |                      |  setToList  O(N)      |                              |
    -------------------------------------------------------------------------------
-}

-- ################################################### IMPLEMENTACIÓN ###################################################

consEmpresa :: Empresa
-- PROPÓSITO: Construye una empresa vacía.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se utiliza la operación "emptyM" de costo constante en el peor caso.
consEmpresa = ConsE EmptyM EmptyM


buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- PROPÓSITO: Devuelve el empleado con dicho CUIL.
-- PRECONDICIÓN: El CUIL es de un empleado de la empresa.
-- COSTO: O(log C).
    -- Siendo C la cantidad de CUIL, se utiliza la operación "lookupM" de costo "log C". Es por eso que el costo total
    -- de la función en el caso promedio es "log C".
buscarPorCUIL c (ConsE mss mce) = fromJust (lookupM c mce)


empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- PROPÓSITO: Indica los empleados que trabajan en un sector dado.
-- COSTO: O(log S + E).
    -- Siendo S la cantidad de Sectores y E la cantidad de empleados, se utiliza la operación lookupM de costo "log S", y la 
    -- operación "setToList" de costo lineal en E.
empleadosDelSector s (ConsE mss mce) = case lookupM s mss of 
                                         Just e -> setToList empleados
                                         _      -> []


todosLosCUIL :: Empresa -> [CUIL]
-- PROPÓSITO: Indica todos los CUIL de empleados de la empresa.
-- COSTO: O(C).
    -- Siendo C la cantidad de CUIL, se utiliza la operación "keys" de costo lineal en C. Es por eso que el costo total de la
    -- función en el caso promedio es lineal.
todosLosCUIL (ConsE mss mce) = keys mce


todosLosSectores :: Empresa -> [SectorId]
-- PROPÓSITO: Indica todos los sectores de la empresa.
-- COSTO: O(S).
    -- Siendo S la cantidad de Sectores, se utiliza la operación "keys" de costo lineal en S. Es por eso que el costo total de 
    -- la función en el caso promedio es lineal.
todosLosSectores (ConsE mss mce) = keys mss


agregarSector :: SectorId -> Empresa -> Empresa
-- PROPÓSITO: Agrega un sector a la empresa, inicialmente sin empleados.
-- COSTO: O(log S).
    -- Siendo S la cantidad de Sectores, se utiliza la operación "assocM" de costo "log S". Es por eso que el costo total de la
    -- función en el caso promedio es "log S".
agregarSector s (ConsE mss mce) = let nMSS = assocM s EmptyS mss
                                   in ConsE nMss mce


agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- PROPÓSITO: Agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el CUIL dado.
-- COSTO: O(S log S + log E + log C).
    -- Siendo S la cantidad de Sectores y E la cantidad de empleados, se utilizan las funciones "empleadoConSectores" de costo
    -- "S log S" y "agregarEmpleado'" de costo "S log S + log E", además de la operación "assocM" de costo "log C". Es por eso
    -- que el costo total de la función en el caso promedio es "S log S + log E + log C".
agregarEmpleado ss c (ConsE mss mce) = let e = empleadoConSectores ss (consEmpleado c);
                                           nMSS = agregarEmpleado' ss e mss; 
                                           nMCE = assocM c e mce
                                        in ConsE nMss nMCE


agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
-- PROPÓSITO: Agrega un sector al empleado con dicho CUIL.
-- COSTO: O(log S + log C + log E).
    -- Siendo S la cantidad de Sectores, C la cantidad de CUIL y E la cantidad de Empleados, se realizan las operaciones "lookupM"
    -- de costo "log C" y "log S", "assocM" de costo "log C" y "log S", "addS" de costo "log E". Es por eso que el costo total
    -- de la función en el caso promedio es "log S + log C + log E". 
agregarASector s c (ConsE mss mce) = case lookupM c mce of
                                       Just e -> let e' = incorporarSector s e; 
                                                     nMCE = assocM c e' mce
                                                  in case lookupM s mss of
                                                       Just se -> let nMSS = assocM s (addS e' se) mss
                                                                   in ConsE nMSS nMCE
                                                       _       -> let nMSS = assocM s (addS e' emptyS) mss
                                                                   in ConsE nMSS nMCE
                                       _      -> ConsE mss mce


borrarEmpleado :: CUIL -> Empresa -> Empresa
-- PROPÓSITO: Elimina al empleado que posee dicho CUIL.
-- COSTO: O(S log S + log C + log E).
    -- Siendo S la cantidad de Sectores, C la cantidad de CUIL y E la cantidad de Empleados, se realizan las operaciones "lookupM"
    -- y "deleteM" ambas de costo "log C", además de la función "eliminarEmpleado" de costo "S log S + log E" y "sectores" de costo
    -- "S". Es por eso que el costo total de la función en el caso promedio es "S log S + log C + log E".
borrarEmpleado c (ConsE mss mce) = case lookupM c mce of
                                       Just e -> let nMCE = deleteM c mce;
                                                     nMSS = eliminarEmpleado e (sectores e) mss
                                                  in ConsE nMSS nMCE
                                       _      -> ConsE mss mce


-- #################################################### AUXILIARES ####################################################

fromJust :: Maybe a -> a
-- PROPÓSITO: Devuelve el valor del dato dado.
-- PRECONDICIÓN: El valor debe existir.
-- COSTO: O(1).
    -- Siendo de costo constante ya que solamente se realiza pattern matching y se devuelve un valor que se accedió.
fromJust (Just x) = x
fromJust _        = error "Se incumplio la precondicion."


empleadoConSectores :: [SectorId] -> Empleado -> Empleado
-- PROPÓSITO: Agrega la lista de sectores dada al empleado dado.
-- COSTO: O(S log S).
    -- Siendo S la cantidad de sectores, por cada sector se utiliza la función "incorporarSector" de costo "log S". Es por eso
    -- que el costo total de la función en el caso promedio es "S log S".
empleadoConSectores []     e = e
empleadoConSectores (s:ss) e = incorporarSector s (empleadoConSectores ss)

agregarEmpleado' :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado) 
-- PROPÓSITO: Agrega al empleado dado a la lista de sectores dada en el Map dado.
-- COSTO: O(S log S + log E).
    -- Siendo S la cantidad de sectores y E la cantidad de empleados, por cada sector se realiza la operación "lookupM" y "assocM"
    -- ambas de costo "log S", además de la operación "addS" de costo "log E". Es por eso que el costo total de la función en el
    -- caso promedio es "S log S + log E".
agregarEmpleado' []     _ m = m 
agregarEmpleado' (s:ss) e m = case lookupM s m of
                                Just se -> assocM s (addS e se) (agregarEmpleado' ss e m)
                                _       -> agregarEmpleado' ss e m


eliminarEmpleado :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
-- PROPÓSITO: Describe el Map dado sin el empleado en la lista de SectorId dada.
-- PRECONDICIÓN: La lista de SectorId debe existir en el Map.
-- COSTO: O(S log S + log E).
    -- Siendo S la cantidad de sectores y E la cantidad de Empleados, se utilizan las operaciones "lookupM" y "assocM" de costo
    -- "log S", además de "removeS" de costo "log E". Es por eso que el costo total de la función en el caso promedio es "S log S + log E".
eliminarEmpleado e []     m = m
eliminarEmpleado e (s:ss) m = case lookupM s m of
                                Just se -> assocM s (removeS e se) (eliminarEmpleado e ss m)
                                _       -> error "No debería pasar, todos los sectores de la lista existen"