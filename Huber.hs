-- PUNTO 1
data Chofer = Chofer {
    nombre :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condicion :: Condicion
}

type Condicion = Viaje -> Bool

type Nombre = String
type Direccion = String

data Viaje = Viaje {
    fecha :: String,
    cliente :: (Nombre, Direccion),
    costo :: Int
}

-- PUNTO 2
tomaCualquierViaje :: Condicion
tomaCualquierViaje unViaje = True

valeMasDe :: Int -> Condicion
valeMasDe unCosto unViaje =  (>) (costo unViaje) unCosto

masDeNLetras :: Int -> Condicion
masDeNLetras n unViaje = (length.fst.cliente) unViaje > n

noVivaEn :: String -> Condicion
noVivaEn unLugar unViaje = (snd.cliente) unViaje /= unLugar

--PUNTO 3
lucas :: (Nombre, Direccion)
lucas = ("Lucas","Victoria")

daniel :: Chofer
daniel = Chofer {
    nombre = "Daniel",
    kilometraje = 23500,
    viajes = [Viaje "20/04/2017" lucas 150],
    condicion = noVivaEn "Olivos"
}

alejandra :: Chofer 
alejandra = Chofer {
    nombre = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condicion = tomaCualquierViaje
}
-- PUNTO 4
puedeTomar :: Viaje -> Chofer -> Bool
puedeTomar unViaje unChofer = condicion unChofer unViaje
-- PUNTO 5
liquidacionDe :: Chofer -> Int
liquidacionDe = sum.map costo.viajes

-- PUNTO 6
realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje unViaje = efectuarViaje unViaje . menorViaje .filter (puedeTomar unViaje) 

menorViaje :: [Chofer] -> Chofer
menorViaje [] = undefined
menorViaje [unChofer] = unChofer
menorViaje (x:y:xs)
    | (length.viajes) x < (length.viajes) y = menorViaje (x:xs)
    | otherwise = menorViaje (y:xs)

efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje unViaje unChofer = unChofer {viajes = unViaje : viajes unChofer} -- Para mi los viajes tambien deberían incluir la distancia como tal, para poder agregarsela al kilometraje del chofer cuando este realizar dicho viaje


-- PUNTO 7
-- a)
repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje    

nitoInfy :: Chofer
nitoInfy = Chofer {
    nombre = "Nito Infy",
    kilometraje = 70000,
    viajes = repetirViaje (Viaje "11/03/2017" lucas 50),
    condicion = masDeNLetras 2
}

{-
b) No se puede saber la liquidacion de Nito debido a que la función liquidacionDe suma el costo de TODOS los viajes que hizo el chofer, y como el mismo realizó infinitos viajes, tendría una cantidad interminable de viajes a cobrar, por lo que no se puede determinar la cifra que terminaría cobrando el chofer.
c) Aunque Nito tiene una cantidad infinita de viajes, como el viaje de Lucas cumple con la condición que impone nito, el chofer va a aceptar el viaje y lo encabezará en su lista de infinitos viajes.
-}

-- PUNTO 8
gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng arg1 arg2 arg3 = 
     max arg1 . head . filter arg2 . map arg3
