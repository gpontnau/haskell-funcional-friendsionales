module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


data Persona = Persona {
    nombre       :: String,
    edad         :: Number,
    alegronios   :: NivelAlegria,
    nerviofrinas :: NivelAnsiedad,
    tareas       :: [Persona -> Persona]
} deriving Show

type NivelAlegria = Number
type NivelAnsiedad = Number

-- Para las personas que son más alegres que ansiosas, es decir que su nivel de 
-- alegronios es superior al de nerviofrinas, su energía es el doble de los alegronios 
-- y no más de 340 yulius.

-- Si por el contrario es más ansioso que alegre y es menos de 40 años, la energía es 
-- 300 yulius menos el nivel de estrés de una persona. El mismo equivale al nivel de 
--amsiedad de la persona pero se multiplica por 1.5 si es que tiene más de 5 tareas pendientes.
esJoven :: Persona -> Bool
esJoven = (< 40) . edad

nivelDeEnergia :: Persona -> Number
nivelDeEnergia persona
    | alegronios persona > nerviofrinas persona = (min 340) . ((*2) . alegronios) $ persona --masAlegreQueAnsiosa
    | esJoven persona && (nerviofrinas persona > alegronios persona) = (300 -) . nivelDeEstres $ persona -- masAnsiosaQueAlegra
    | otherwise = (10+) . alegronios $ persona


nivelDeEstres :: Persona -> Number
nivelDeEstres persona
    | (5<) . length . tareas $ persona   = nerviofrinas persona * 1.5
    | otherwise = nerviofrinas persona

-- 2

cuantoDueleVerLasBuenas :: [Persona] -> Bool
cuantoDueleVerLasBuenas grupoPersonas = all esVital grupoPersonas

esVital :: Persona -> Bool
esVital persona =  (not.esJoven) persona && ((>100).nivelDeEnergia) persona

nivelTotalDeAnsiedad :: [Persona] -> Number
nivelTotalDeAnsiedad = sumOf nerviofrinas

losMasCriticados :: (Persona->Bool) -> [Persona] ->  [String]
losMasCriticados criterio = map nombre.take 2.filter criterio

-- 3

--Vamos a modelar las tareas. Cada vez que se realiza una tarea se descomprime a la persona. Esto hace que baje en 10 nerviofrinas su nivel amsiedad luego de realizar la misma. El mínimo valor de amsiedad es cero.

-- baja10NerviofrinasMin0 :: Number -> Number
-- baja10NerviofrinasMin0 unNumero = max 0. (+10) . (*10) $ unNumero

-- --test de codearUnProyectoNuevo
-- juan = Persona{nombre="juan", edad=10, nivelDeAlegria=100, nivelDeAnsiedad=100,tareas= [codearUnProyectoNuevo] }

-- codearUnProyectoNuevo :: Persona->Persona
-- codearUnProyectoNuevo persona = persona{nivelDeAlegria = nivelDeAlegria persona+110,nivelDeAnsiedad = baja10NerviofrinasMin0.(+50).nivelDeAnsiedad $ persona }


-- --test de hacerTramitesEnAfip
-- cecilia = Persona{nombre="cecilia", edad=10, nivelDeAlegria=40, nivelDeAnsiedad=40,tareas= [hacerTramitesEnAfip 2] }
-- juan' = Persona{nombre="juan'", edad=10, nivelDeAlegria=100, nivelDeAnsiedad=250,tareas= [hacerTramitesEnAfip 2] }

-- hacerTramitesEnAfip :: Number -> Persona -> Persona
-- hacerTramitesEnAfip cantidadDeTramites persona = persona{ nivelDeAnsiedad = baja10NerviofrinasMin0.(max 300).(*cantidadDeTramites).nivelDeAnsiedad $ persona }


-- --test de andarEnBici
-- cecilia'= Persona{nombre="cecilia'", edad=10, nivelDeAlegria=90, nivelDeAnsiedad=65,tareas= [andarEnBici 1] }

-- andarEnBici :: Number -> Persona -> Persona
-- andarEnBici km persona = persona{ nivelDeAnsiedad = 0,nivelDeAlegria =((50*km)+).nivelDeAlegria $ persona}


-- --test de escucharMusica
-- santiago = Persona{nombre="santiago", edad=10, nivelDeAlegria=40, nivelDeAnsiedad=30,tareas= [hacerTramitesEnAfip 2] }

-- escucharMusica :: Persona -> Persona
-- escucharMusica persona = persona{nivelDeAnsiedad = baja10NerviofrinasMin0.(10 `subtract` ).nivelDeAnsiedad $ persona}


-- -- Punto 4

-- --test de punto 4
-- tareasA1 = [ escucharMusica, andarEnBici 2] 

-- energiaResultante :: Persona -> [Persona-> Persona] -> Persona
-- energiaResultante persona tareas = foldr ($) persona tareas

-- -- Punto 5

-- -- hiceLoQuePude     ___ PRACTICAR  RECURSIVIDAD