-- PRIMERA PARTE
-- PUNTO 1
data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}

type Gema = Personaje -> Personaje

data Personaje = Personaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: Planeta  
}

type Habilidad = String
type Planeta = String

estaCompleto :: Guantelete -> Bool
estaCompleto unGuantelete = length (gemas unGuantelete) == 6 && material unGuantelete == "uru"

type Universo = [Personaje]

chasquearUniverso :: Universo -> Guantelete -> Universo
chasquearUniverso unUniverso unGuantelete 
    | estaCompleto unGuantelete = take (div (length unUniverso) 2) unUniverso
    | otherwise = unUniverso

-- PUNTO 2 
aptoParaPendex :: Universo -> Bool
aptoParaPendex = any ((<45) . edad)  

--energiaIgualSumatoria :: Universo -> Bool
--energiaIgualSumatoria                       -- NO SE ME OCURRE COMO APLICAR ORDEN SUPERIOR EN ESTE PUNTO

-- SEGUNDA PARTE
-- PUNTO 3
mente :: Int -> Gema
mente = modificarEnergia (+) 

modificarEnergia :: (Int -> Int -> Int) -> Int -> Personaje -> Personaje
modificarEnergia operacion cantidad unPersonaje = unPersonaje {energia = operacion (energia unPersonaje) cantidad}

alma :: Habilidad -> Gema
alma unaHabilidad = modificarEnergia (-) 10 . eliminarHabilidad unaHabilidad

eliminarHabilidad :: Habilidad -> Personaje -> Personaje
eliminarHabilidad unaHabilidad unPersonaje = unPersonaje {habilidades = filter (/= unaHabilidad) (habilidades unPersonaje)}

espacio :: Planeta -> Gema
espacio nuevoPlaneta unPersonaje = unPersonaje {planeta = nuevoPlaneta} 

poder :: Gema
poder unPersonaje 
    | (length.habilidades) unPersonaje < 2 = modificarEnergia (-) (energia unPersonaje).borrarHabilidades $ unPersonaje
    | otherwise = modificarEnergia (-) (energia unPersonaje) unPersonaje

borrarHabilidades :: Personaje -> Personaje
borrarHabilidades unPersonaje = unPersonaje {habilidades = []}

tiempo :: Gema
tiempo unPersonaje 
    | edad unPersonaje > 36 = modificarEdad (div (edad unPersonaje) 2) unPersonaje 
    | otherwise = modificarEdad 18 unPersonaje

modificarEdad :: Int -> Personaje -> Personaje
modificarEdad nuevaEdad unPersonaje = unPersonaje {edad = nuevaEdad}

gemaLoca :: Gema -> Gema
gemaLoca unaGema = unaGema.unaGema

-- PUNTO 4

guanteleteFalopa :: Guantelete
guanteleteFalopa = Guantelete {
    material = "Goma",
    gemas = [tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")]
}

-- PUNTO 5
type Enemigo = Personaje

utilizar :: [Gema] -> Enemigo -> Enemigo
utilizar gemas unEnemigo = foldl (\unEnemigo unaGema -> unaGema unEnemigo) unEnemigo gemas

-- PUNTO 6
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa unGuantelete unPersonaje = gemaDeMayorPoder unPersonaje .gemas $ unGuantelete

gemaDeMayorPoder :: Personaje -> [Gema] -> Gema
gemaDeMayorPoder _ [] = undefined                   -- Linea innecesaria, haskell lloraba con que el pattern matching no era exhaustivo
gemaDeMayorPoder _ [x] = x
gemaDeMayorPoder unPersonaje (x:y:xs) 
    | energiaDespuesDeUnaGema x unPersonaje < energiaDespuesDeUnaGema y unPersonaje = gemaDeMayorPoder unPersonaje (x:xs)
    | otherwise = gemaDeMayorPoder unPersonaje (y:xs)

energiaDespuesDeUnaGema :: Gema -> Personaje -> Int
energiaDespuesDeUnaGema unaGema unPersonaje = energia unPersonaje - (energia.unaGema) unPersonaje

-- PUNTO 7
{-
a) No se podría averiguar el resultado de la funcion gemaMasPoderosa aplicada en punisher con el guanteleteDeLocos debido a que la misma debe recorrer TODAS las gemas para determinar cual es la de máximo poder, y como la lista de gemas es infinita, nunca encontraría la gema final.
b) Si se podría ver el resultado de aplicar usoLasTresPrimerasGemas guanteleteDeLocos punisher ya que la función solo toma los primeros 3 elementos de la lista infinita para poder operar, y como haskell funciona bajo lazy evaluation, una vez que take obtiene los primeros 3 elementos de la lista infinita de gemas, ya puede saltar a la función utilizar, y como esta va a trabajar con una cantidad finita de gemas, arrojará un resultado final.
-}
