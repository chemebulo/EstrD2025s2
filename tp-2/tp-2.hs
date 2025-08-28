-- EJERCICIO 1: Recursión sobre listas.

-- EJERCICIO 1.1:

sumatoria :: [Int] -> Int
-- PROPÓSITO: Dada una lista de enteros devuelve la suma de to dos sus elementos.
sumatoria []     = 0 
sumatoria (n:ns) = n + sumatoria ns


-- EJERCICIO 1.2:

longitud :: [a] -> Int
-- PROPÓSITO: Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud []     = 0
longitud (_:xs) = 1 + longitud xs  


-- EJERCICIO 1.3:

sucesores :: [Int] -> [Int]
-- PROPÓSITO: Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns 


-- EJERCICIO 1.4:

conjuncion :: [Bool] -> Bool
-- PROPÓSITO: Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion []     = True 
conjuncion (b:bs) = b && conjuncion bs 


-- EJERCICIO 1.5:

disyuncion :: [Bool] -> Bool
-- PROPÓSITO: Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs


-- EJERCICIO 1.6:

aplanar :: [[a]] -> [a]
-- PROPÓSITO: Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar []       = []
aplanar (xs:xss) = xs ++ aplanar xss


-- EJERCICIO 1.7:

pertenece :: Eq a => a -> [a] -> Bool
-- PROPÓSITO: Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs 


-- EJERCICIO 1.8:

apariciones :: Eq a => a -> [a] -> Int
-- PROPÓSITO: Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones e []     = 0
apariciones e (x:xs) = if e == x
                          then 1 + apariciones e xs
                          else apariciones e xs
 

-- EJERCICIO 1.9:

losMenoresA :: Int -> [Int] -> [Int]
-- PROPÓSITO: Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA n []     = []
losMenoresA n (x:xs) = if x < n
                          then x : losMenoresA n xs
                          else losMenoresA n xs


-- EJERCICIO 1.10:

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
-- PROPÓSITO: Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA n []       = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n
                                    then xs : lasDeLongitudMayorA n xss
                                    else lasDeLongitudMayorA n xss 


-- EJERCICIO 1.11:

agregarAlFinal :: [a] -> a -> [a]
-- PROPÓSITO: Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal []     y = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y


-- EJERCICIO 1.12:

agregar :: [a] -> [a] -> [a]
-- PROPÓSITO: Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--            elementos de la segunda a continuación. Definida en Haskell como (++).
agregar []     ys = ys
agregar (x:xs) ys = x : agregar xs ys


-- EJERCICIO 1.13:

reversa :: [a] -> [a]
-- PROPÓSITO: Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa []     = []
reversa (x:xs) = reversa xs ++ [x] 


-- EJERCICIO 1.14:

zipMaximos :: [Int] -> [Int] -> [Int]
-- PROPÓSITO: Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el
--            elemento n de la primera lista y de la segunda lista, teniendo en cuenta que las listas no necesariamente 
--            tienen la misma longitud
zipMaximos []     ys     = ys 
zipMaximos xs     []     = xs 
zipMaximos (x:xs) (y:ys) = if x > y
                              then x : zipMaximos xs ys 
                              else y : zipMaximos xs ys

-- EJERCICIO 1.15:

elMinimo :: Ord a => [a] -> a
-- PROPÓSITO: Dada una lista devuelve el mínimo.
-- PRECONDICIÓN: La lista no es vacía.
elMinimo []     = error "No existe un elemento mínimo de una lista vacía."
elMinimo (x:[]) = x
elMinimo (x:xs) = if x < elMinimo xs
                     then x
                     else elMinimo xs


-- ####################################################################################################################### --

-- EJERCICIO 2: Recursión sobre números.

-- EJERCICIO 2.1:

factorial :: Int -> Int
-- PROPÓSITO: Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. 
--            Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial 0 = 1
factorial n = n * factorial (n-1)


-- EJERCICIO 2.2:

cuentaRegresiva :: Int -> [Int]
-- PROPÓSITO: Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). 
--            Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva n = if n < 1
                       then []
                       else n : cuentaRegresiva (n-1)

-- EJERCICIO 2.3:

repetir :: Int -> a -> [a]
-- PROPÓSITO: Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir 0 _ = []
repetir n e = e : repetir (n-1) e


-- EJERCICIO 2.4:

losPrimeros :: Int -> [a] -> [a]
-- PROPÓSITO: Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs. 
--            Si la lista es vacía, devuelve una lista vacía.
losPrimeros 0 _      = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs


-- EJERCICIO 2.5:

sinLosPrimeros :: Int -> [a] -> [a]
-- PROPÓSITO: Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. 
--            Si n es cero, devuelve la lista completa.
sinLosPrimeros 0 xs     = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (_:xs) = sinLosPrimeros (n-1) xs


-- ####################################################################################################################### --

-- EJERCICIO 3: Registros.

data Persona = ConsPersona String Int
    deriving Show


-- EJRCICIO 3.1.1:

mayoresA :: Int -> [Persona] -> [Persona]
-- PROPÓSITO: Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA n []     = []
mayoresA n (p:ps) = if edad p > n
                       then p : mayoresA n ps
                       else mayoresA n ps

edad :: Persona -> Int
-- PROPÓSITO: Describe la edad de la persona dada.
edad (ConsPersona _ e) = e


-- EJRCICIO 3.1.2:

promedioEdad :: [Persona] -> Int
-- PROPÓSITO: Dada una lista de personas devuelve el promedio de edad entre esas personas.
-- PRECONDICIÓN: La lista al menos posee una persona.
promedioEdad ps = div (sumatoriaEdades ps) (longitud ps)  

sumatoriaEdades :: [Persona] -> Int
-- PROPÓSITO: Describe la sumatoria de las edades de todas las personas de la lista dada.
sumatoriaEdades []     = 0
sumatoriaEdades (p:ps) = edad p + sumatoriaEdades ps


-- EJRCICIO 3.1.3:

elMasViejo :: [Persona] -> Persona
-- PROPÓSITO: Dada una lista de personas devuelve la persona más vieja de la lista.
-- PRECONDICIÓN: La lista al menos posee una persona.
elMasViejo []     = error "La lista no puede ser vacía."
elMasViejo (p:[]) = p
elMasViejo (p:ps) = if edad p > edad (elMasViejo ps)
                       then p
                       else elMasViejo ps


-- EJERCICIO 3.2:

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon = ConsPokemon TipoDePokemon Int
    deriving Show

data Entrenador = ConsEntrenador String [Pokemon]
    deriving Show


-- EJERCICIO 3.2.1:

cantPokemon :: Entrenador -> Int
-- PROPÓSITO: Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon (ConsEntrenador _ ps) = cantidadDePokemon ps

cantidadDePokemon :: [Pokemon] -> Int
-- PROPÓSITO: Describe la cantidad de Pokémon hay en la lista de Pokémon dada.
cantidadDePokemon []     = 0
cantidadDePokemon (p:ps) = 1 + cantidadDePokemon ps


-- EJERCICIO 3.2.2:

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
-- PROPÓSITO: Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
-- Lo hice con la subtarea "pokemonDeTipoEn" porque en el punto 3.2.3 la reutilizo. Se que hubiera sido mejor hacer una subtarea que cuente los del tipo dado.
cantPokemonDe t (ConsEntrenador _ ps) = longitud (pokemonDeTipoEn t ps) 

pokemonDeTipoEn :: TipoDePokemon -> [Pokemon] -> [Pokemon]
-- PROPÓSITO: Describe todos los Pokémon del tipo dado en la lista de Pokémon dada.
pokemonDeTipoEn _ []     = []
pokemonDeTipoEn t (p:ps) = if esMismoTipo t (tipoDe p)
                              then p : pokemonDeTipoEn t ps
                              else pokemonDeTipoEn t ps

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
-- PROPÓSITO: Describe si los dos tipos de Pokémon dados son iguales.
esMismoTipo Agua   Agua   = True
esMismoTipo Fuego  Fuego  = True
esMismoTipo Planta Planta = True
esMismoTipo _      _      = False

tipoDe :: Pokemon -> TipoDePokemon
-- PROPÓSITO: Describe el tipo de Pokémon del Pokémon dado.
tipoDe (ConsPokemon t _) = t


-- EJERCICIO 3.2.3:

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
-- PROPÓSITO: Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al primer entrenador,
--            que le ganarían a todos los Pokemon del segundo entrenador.
-- PRECONDICIÓN: Debe existir al menos un Pokémon del tipo dado en el primer entrenador dado.
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ ps1) (ConsEntrenador _ ps2) = cuantosDeTipoLeGanarianA (pokemonDeTipoEn t ps1) ps2

cuantosDeTipoLeGanarianA :: [Pokemon] -> [Pokemon] -> Int
-- PROPÓSITO: Describe la cantidad de Pokémon de la primera lista de Pokémon dada le gana a todos los Pokémon de la segunda lista dada.
cuantosDeTipoLeGanarianA []       _   = 0
cuantosDeTipoLeGanarianA (p1:ps1) ps2 = if leGanaATodos p1 ps2
                                           then 1 + cuantosDeTipoLeGanarianA ps1 ps2
                                           else cuantosDeTipoLeGanarianA ps1 ps2

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
-- PROPÓSITO: Indica si el Pokémon dado le gana a todos los Pokémon de la lista dada.
leGanaATodos _  []       = True
leGanaATodos p1 (p2:ps2) = superaA p1 p2 && leGanaATodos p1 ps2

superaA :: Pokemon -> Pokemon -> Bool
-- PROPÓSITO: Indica si el primer Pokémon dado es supera al segundo Pokémon dado.
superaA (ConsPokemon t1 _) (ConsPokemon t2 _) = esSuperiorQue t1 t2

esSuperiorQue :: TipoDePokemon -> TipoDePokemon -> Bool
-- PROPÓSITO: Indica si el primer tipo de Pokémon dado es superior al segundo tipo de Pokémon dado.
esSuperiorQue Agua   Fuego  = True
esSuperiorQue Fuego  Planta = True
esSuperiorQue Planta Agua   = True
esSuperiorQue _      _      = False


-- EJERCICIO 3.2.4:

esMaestroPokemon :: Entrenador -> Bool
-- PROPÓSITO: Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon (ConsEntrenador _ ps) = hayPokemonDeCadaTipoEn ps

hayPokemonDeCadaTipoEn :: [Pokemon] -> Bool
-- PROPÓSITO: Describe si hay un Pokémon de cada tipo posible en la lista de Pokémon dada.
hayPokemonDeCadaTipoEn ps = hayPokemonTipoEn Agua   ps && 
                            hayPokemonTipoEn Fuego  ps && 
                            hayPokemonTipoEn Planta ps 

hayPokemonTipoEn :: TipoDePokemon -> [Pokemon] -> Bool
-- PROPÓSITO: Indica si hay algún Pokémon del tipo dado en la lista de Pokémon dado.
hayPokemonTipoEn t []     = False
hayPokemonTipoEn t (p:ps) = (esMismoTipo t (tipoDe p)) || hayPokemonTipoEn t ps


-- EJERCICIO 3.3:

data Seniority = Junior | SemiSenior | Senior
    deriving Show

data Proyecto = ConsProyecto String
    deriving Show

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show

data Empresa = ConsEmpresa [Rol]
    deriving Show


-- EJERCICIO 3.3.1:

proyectos :: Empresa -> [Proyecto]
-- PROPÓSITO: Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos (ConsEmpresa rs) = proyectosEn rs

proyectosEn :: [Rol] -> [Proyecto]
-- PROPÓSITO: Describe todos los proyectos de la lista de roles dada.
proyectosEn []     = []
proyectosEn (r:rs) = if not (estaProyectoEn (proyectoDe r) (proyectosEn rs))
                        then proyectoDe r : proyectosEn rs
                        else proyectosEn rs

estaProyectoEn :: Proyecto -> [Proyecto] -> Bool
-- PROPÓSITO: Indica si el proyecto dado se encuentra en la lista de proyectos dados.
estaProyectoEn _  []       = False
estaProyectoEn p1 (p2:ps2) = esMismoProyecto p1 p2 || estaProyectoEn p1 ps2

esMismoProyecto :: Proyecto -> Proyecto -> Bool
-- PROPÓSITO: Indica si ambos proyectos dados son el mismo.
esMismoProyecto (ConsProyecto n1) (ConsProyecto n2) = n1 == n2

proyectoDe :: Rol -> Proyecto
-- PROPÓSITO: Describe el proyecto del rol dado.
proyectoDe (Developer  _ p) = p
proyectoDe (Management _ p) = p


-- EJERCICIO 3.3.2:

losDevSenior :: Empresa -> [Proyecto] -> Int
-- PROPÓSITO: Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además
--            a los proyectos dados por parámetro.
losDevSenior (ConsEmpresa rs) ps = cantidadDeSeniorDeQueParticipanEn rs ps

cantidadDeSeniorDeQueParticipanEn :: [Rol] -> [Proyecto] -> Int
-- PROPÓSITO: Describe la cantidad de desarrolladores senior que hay en la lista de roles y que además participan de la lista de proyectos dada.
cantidadDeSeniorDeQueParticipanEn []     _  = 0
cantidadDeSeniorDeQueParticipanEn (r:rs) ps = if esSenior r && participaEnAlgunProyecto r ps
                                                 then 1 + cantidadDeSeniorDeQueParticipanEn rs ps
                                                 else cantidadDeSeniorDeQueParticipanEn rs ps

esSenior :: Rol -> Bool
-- PROPÓSITO: Indica si el rol dado es senior.
esSenior (Developer  Senior _) = True
esSenior (Management Senior _) = True
esSenior _                     = False

participaEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
-- PROPÓSITO. Indica si el rol dado participa en algún proyecto de la lista de proyectos dada.
participaEnAlgunProyecto _ []     = False
participaEnAlgunProyecto r (p:ps) = esMismoProyecto (proyectoDe r) p || participaEnAlgunProyecto r ps 


-- EJERCICIO 3.3.3:

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
-- PROPÓSITO: Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn ps (ConsEmpresa rs) = cantidadQueTrabajanEn rs ps

cantidadQueTrabajanEn :: [Rol] -> [Proyecto] -> Int
-- PROPÓSITO: Describe la cantidad de roles que trabajan en alguno de los proyectos dados.
cantidadQueTrabajanEn []     _  = 0
cantidadQueTrabajanEn (r:rs) ps = if participaEnAlgunProyecto r ps
                                     then 1 + cantidadQueTrabajanEn rs ps
                                     else cantidadQueTrabajanEn rs ps

-- EJERCICIO 3.3.4:

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
-- PROPÓSITO: Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorProyectoEn (proyectosEn rs) rs

asignadosPorProyectoEn  :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
-- PROPÓSITO: Describe una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyectoEn []     _  = []
asignadosPorProyectoEn (p:ps) rs = (p, cantidadDeAsingadosEn p rs) : asignadosPorProyectoEn ps rs

cantidadDeAsingadosEn :: Proyecto -> [Rol] -> Int
-- PROPÓSITO: Describe la cantidad de roles que hay asignados en el proyecto dado.
cantidadDeAsingadosEn _ []     = 0
cantidadDeAsingadosEn p (r:rs) = if esMismoProyecto (proyectoDe r) p
                                    then 1 + cantidadDeAsingadosEn p rs
                                    else cantidadDeAsingadosEn p rs