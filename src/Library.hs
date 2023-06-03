module Library where
import PdePreludat

-- 1) Modelar naves y agregar una nueva
type Durabilidad = Number
type Escudo = Number
type Ataque = Number
type Poder = Nave -> Nave

data Nave = Nave {
    durabilidad :: Durabilidad,
    escudo :: Escudo,
    ataque :: Ataque,
    poder :: Poder
} deriving (Show, Eq)

tieFighter, xWing, naveDarthVader, milleniumFalcon :: Nave
tieFighter = Nave 200 100 50 turbo
xWing = Nave 300 150 100 reparacionEmergencia
naveDarthVader = Nave 500 300 200 superTurbo
milleniumFalcon = Nave 1000 500 50 (reparacionEmergencia . mejorarEscudos 100)

turbo :: Poder
turbo = disminuirAtaque (-25)

superTurbo :: Poder
superTurbo = dañar 45 . turbo . turbo . turbo

reparacionEmergencia :: Poder
reparacionEmergencia = mejorarEscudos 50 . disminuirAtaque 30

dañar :: Number -> Nave -> Nave
dañar n nave = nave{durabilidad = restaNoNegativa (durabilidad nave) n}

mejorarEscudos :: Number -> Nave -> Nave
mejorarEscudos n nave = nave{escudo = escudo nave + n}

disminuirAtaque :: Number -> Nave -> Nave
disminuirAtaque n nave = nave{ataque = restaNoNegativa (ataque nave) n}

restaNoNegativa :: Number -> Number -> Number
restaNoNegativa n m = max 0 (n - m)

-- 2) Calcular durabilidad total de una flota
type Flota = [Nave]

durabilidadTotal :: Flota -> Durabilidad
durabilidadTotal flota = sum (map durabilidad flota)

-- 3) Saber cómo queda una nave luego de ser atacada por otra
resultadoAtaque :: Nave -> Nave -> Nave
resultadoAtaque atacante atacada = atacar (potenciar atacante) (potenciar atacada)

atacar :: Nave -> Nave -> Nave
atacar atacante atacada =  dañar (daño atacante atacada) atacada

daño :: Nave -> Nave -> Number
daño atacante atacada = restaNoNegativa (ataque atacante) (escudo atacada)

potenciar :: Nave -> Nave
potenciar nave = (poder nave) nave


-- 4) Averiguar si una nave está fuera de combate
fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0


-- 5) Averiguar cómo queda una flota enemiga luego de realizar una misión sorpresa con una nave siguiendo una estrategia.
type Estrategia = Nave -> Bool

debil :: Estrategia
debil nave = escudo nave < 200

peligrosa :: Number -> Estrategia
peligrosa n nave = ataque nave > n

fueraDeCombateAlSerAtacada :: Nave -> Estrategia
fueraDeCombateAlSerAtacada atacante = fueraDeCombate . resultadoAtaque atacante

misionSorpresa :: Estrategia -> Nave -> Flota ->  Flota
misionSorpresa estrategia nave = mapSelectivo (atacar nave) estrategia 

mapSelectivo :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapSelectivo cambio condicion lista = map cambio (filter condicion lista) ++ filter (not.condicion) lista

-- 6) Considerando una nave y una flota enemiga en particular, dadas dos estretegias, determinar cuál de ellas es la que minimiza la durabilidad total de la flota atacada y llevar adelante una misión con ella.

mejorEstrategia :: Estrategia -> Estrategia -> Nave -> Flota -> Estrategia
mejorEstrategia  estrategia1 estrategia2 nave flota
    | durabilidadTotal (misionSorpresa estrategia1 nave flota ) < durabilidadTotal (misionSorpresa estrategia1 nave flota ) = estrategia1
    | otherwise = estrategia2 

atacarConMejorEstrategia :: Estrategia -> Estrategia -> Nave -> Flota -> Flota
atacarConMejorEstrategia estrategia1 estrategia2 nave flota = 
    misionSorpresa (mejorEstrategia estrategia1 estrategia2 nave flota) nave flota

-- 7) Construir una flota infinita de naves. ¿Es posible determinar su durabilidad total? ¿Qué se obtiene como respueta cuando se lleva adelante una misión sobre ella? Justificar conceptualmente.
infinitasNaves :: Flota
infinitasNaves = milleniumFalcon : infinitasNaves


-- Variantes
misionSorpresa1 :: Estrategia -> Nave -> Flota ->  Flota
misionSorpresa1 estrategia nave = mapSelectivo1 (atacar nave) estrategia 

mapSelectivo1 :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapSelectivo1 cambio condicion = map (aplicarSi cambio condicion) 

aplicarSi :: (a -> a) -> (a -> Bool) -> a -> a
aplicarSi cambio condicion elemento 
    | condicion elemento = cambio elemento
    | otherwise = elemento
---------
misionSorpresa2 :: Estrategia -> Nave -> Flota ->  Flota
misionSorpresa2 estrategia nave flota = map (atacarSiVerifica estrategia nave) flota 

atacarSiVerifica :: (Nave -> Bool) -> Nave -> Nave -> Nave
atacarSiVerifica estrategia atacante atacada  
    | estrategia atacada = atacar atacante atacada
    | otherwise = atacada

-----------
misionSorpresa3 :: Estrategia -> Nave -> Flota ->  Flota
misionSorpresa3 estrategia _ [] = []
misionSorpresa3 estrategia atacante (atacada:atacadas)
    | estrategia atacada = atacar atacante atacada:cola
    | otherwise = cola
        where cola = misionSorpresa3 estrategia atacante atacadas
