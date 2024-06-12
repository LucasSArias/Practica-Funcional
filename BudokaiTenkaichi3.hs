import Text.Show.Functions
import Data.List

-- Funciones que tal vez te pueden servir, tal vez no

-- Main*> :t takeWhile
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- Main*> takeWhile even [2,4,6,5,6,7,8,9]
-- [2,4,6]

-- Main*> :t genericLength
-- genericLength :: Num i => [a] -> i
-- Main*> genericLength [2,4,6,5,6,7,8,9]
-- 8

-- Probando que ande el git
-- PUNTO 1
data Guerrero = Guerrero {
    nombre :: String,
    ki :: Float,
    raza :: Raza,
    cansancio :: Float,
    personalidad :: Personalidad
}

data Personalidad = Sacado | Perezoso | Tramposo  deriving Eq

data Raza = Humano | Namekiano | Saiyajin deriving Eq

gohan :: Guerrero
gohan = Guerrero {
    nombre = "Gohan",
    ki = 10000,
    raza = Saiyajin,
    cansancio = 0,
    personalidad = Perezoso
}

-- PUNTO 2
esPoderoso :: Guerrero -> Bool
esPoderoso unGuerrero = ((>8000).ki) unGuerrero && ((==Saiyajin).raza) unGuerrero

-- PUNTO 3

type Ejercicio = Guerrero -> Guerrero

pressDeBanca :: Ejercicio
pressDeBanca = aumentaKiCondicional 90.aumentaCansancioCondicional 100

aumentaKi :: Float -> Guerrero -> Guerrero
aumentaKi cantidad unGuerrero = unGuerrero {ki = ki unGuerrero + cantidad}

aumentaCansancio :: Float -> Guerrero -> Guerrero
aumentaCansancio cantidad unGuerrero = unGuerrero {cansancio = cansancio unGuerrero + cantidad}

flexionesDeBrazo :: Ejercicio
flexionesDeBrazo = aumentaCansancioCondicional 50

saltosAlCajon :: Float -> Ejercicio
saltosAlCajon alturaCajon = aumentaCansancioCondicional (alturaCajon / 5). aumentaKiCondicional (alturaCajon / 10) 

snatch :: Ejercicio
snatch unGuerrero
    | esExperimentado unGuerrero = aumentaCansancioCondicional (cansancio unGuerrero * 0.1) . aumentaKiCondicional (ki unGuerrero * 0.05) $ unGuerrero
    | otherwise                  = aumentaCansancioCondicional 100 unGuerrero

esExperimentado :: Guerrero -> Bool
esExperimentado = (>22000).ki 

estaFresco ::  Guerrero -> Bool
estaFresco unGuerrero = (not.estaCansado) unGuerrero && (not.estaExhausto) unGuerrero

estaCansado :: Guerrero -> Bool
estaCansado  = cansancioMayorAl 44  

estaExhausto :: Guerrero -> Bool
estaExhausto = cansancioMayorAl 72 

cansancioMayorAl :: Float -> Guerrero -> Bool
cansancioMayorAl porcentaje unGuerrero = cansancio unGuerrero > (porcentaje/100) * ki unGuerrero

aumentaKiCondicional :: Float -> Guerrero -> Guerrero
aumentaKiCondicional cantidad unGuerrero
    | estaFresco unGuerrero   = aumentaKi cantidad unGuerrero
    | estaCansado unGuerrero  = aumentaKi (cantidad * 2) unGuerrero
    | estaExhausto unGuerrero = aumentaKi (-0.02 * ki unGuerrero) unGuerrero    -- ABUSO DE NOTACION PORQUE ESTOY AUMENTANDO EL KI CON UN VALOR NEGATIVO PERO NO CONSIDERE QUE SEA LO SUFICIENTEMENTE IMPORTANTE PARA CREAR OTRA FUNCION QUE SE LLAME "reduceKi"

aumentaCansancioCondicional :: Float -> Guerrero -> Guerrero
aumentaCansancioCondicional cantidad unGuerrero
    | estaFresco unGuerrero   = aumentaCansancio cantidad unGuerrero
    | estaCansado unGuerrero  = aumentaCansancio (cantidad * 4) unGuerrero
    | estaExhausto unGuerrero = aumentaCansancio 0 unGuerrero                   -- NO SE ACLARA QUE SE DEBE HACER CON EL CANSANCIO DE UN GUERRERO QUE ESTÁ EXHAUSTO, POR LO QUE ARBITRARIAMENTE DECIDO QUE LE SUME 0

realizarEjercicio :: Ejercicio -> Guerrero -> Guerrero
realizarEjercicio unEjercicio unGuerrero = unEjercicio unGuerrero

type Rutina = [(Ejercicio, Descanso)]
type Descanso = Float

armarRutina :: [Ejercicio] -> Guerrero -> Rutina
armarRutina losEjercicios unGuerrero 
    | es Sacado   unGuerrero = zip losEjercicios . replicate (length losEjercicios) $ 0
    | es Perezoso unGuerrero = zip losEjercicios . replicate (length losEjercicios) $ 5 
    | es Tramposo unGuerrero = seRasca

seRasca :: Rutina
seRasca = [(noHaceNada,0)]

noHaceNada :: Ejercicio
noHaceNada unGuerrero = unGuerrero

es :: Personalidad -> Guerrero -> Bool
es unaPersonalidad unGuerrero = personalidad unGuerrero == unaPersonalidad

-- No se podría conocer la rutina final, debido a que una rutina consta de un ejercicio y del tiempo a descansar tras realizarlo, y como las rutinas tienen estructura de lista de duplas, la forma en la que se asigna el tiempo de descanso a cada dupla es replicando ese tiempo tantas veces como ejercicios haya en la lista de ejercicios, para poder "zipearlo" con la lista de ejercicios. Para saber cuantas veces se va a replicar el tiempo de descanso, es necesario contar con la cantidad de ejercicios, y como esta ultima está indeterminada, la funcion armarRutina se cuelga calculando cuantas veces debe repetir el tiempo a descansar para emparejalor con los infinitos ejercicos en una lista de infinitas tuplas. Por lo cual NO ARROJA UN RESULTADO

-- PUNTO 5
realizarRutina :: Rutina -> Guerrero -> Guerrero
realizarRutina unaRutina unGuerrero = foldl realizarSet unGuerrero unaRutina

realizarSet ::  Guerrero -> (Ejercicio, Descanso) -> Guerrero
realizarSet unGuerrero elSet = descansar (snd elSet) . realizarEjercicio (fst elSet) $ unGuerrero

-- PUNTO 6
descansar :: Float -> Guerrero -> Guerrero
descansar cantidad = reduceCansancio (sumatorial cantidad)     

sumatorial :: Float -> Float
sumatorial 0 = 0
sumatorial 1 = 1
sumatorial n = n + sumatorial (n-1)

reduceCansancio :: Float -> Guerrero -> Guerrero
reduceCansancio cantidad unGuerrero 
    | cansancio unGuerrero - cantidad >= 0  = unGuerrero {cansancio = cansancio unGuerrero - cantidad}
    | otherwise                             = unGuerrero {cansancio = 0}

-- PUNTO 7

cantidadOptimaDeMinutos :: Guerrero -> Float
cantidadOptimaDeMinutos unGuerrero 
    | not.estaCansado $ unGuerrero  = 0
    | otherwise                     =  ((+1).last.takeWhile (estaCansado . flip descansar unGuerrero)) [0..]

