data Personaje = Personaje {
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
}

type Actividad = Personaje -> Personaje
-- PARTE 1
irALaEscuela :: Actividad
irALaEscuela unPersonaje
    | nombre unPersonaje == "lisa" = sumaFelicidad 20 unPersonaje
    | otherwise = restaFelicidad 20 unPersonaje

sumaFelicidad :: Int -> Personaje -> Personaje
sumaFelicidad cantidad unPersonaje = unPersonaje {felicidad = cantidad + felicidad unPersonaje}

restaFelicidad :: Int -> Personaje -> Personaje
restaFelicidad cantidad unPersonaje
    | cantidad > felicidad unPersonaje = unPersonaje {felicidad = 0}
    | otherwise = unPersonaje {felicidad = felicidad unPersonaje - cantidad}

modificarDinero :: (Int -> Int) -> Personaje -> Personaje
modificarDinero operacion unPersonaje = unPersonaje {dinero = operacion.dinero $ unPersonaje }

comerNDonas:: Int -> Actividad
comerNDonas cantidad  = sumaFelicidad (10*cantidad).modificarDinero (flip (-) (10*cantidad)) 

irATrabajar :: String -> Actividad
irATrabajar "escuela elemental" = modificarDinero (+ length "escuela elemental").restaFelicidad 20
irATrabajar unTrabajo = modificarDinero (+ length unTrabajo) 

-- PARTE 2
srBurns :: Personaje
srBurns = Personaje {
    nombre = "Sr Burns",
    dinero = 100000000,
    felicidad = 40
}

type Logro = Personaje -> Bool

serMillonario :: Logro
serMillonario unaPersona = dinero unaPersona > dinero srBurns

alegrarse :: Int -> Logro
alegrarse nivel unaPersona = felicidad unaPersona > nivel

verAKrosti :: Logro
verAKrosti unaPersona = dinero unaPersona >= 10

-- A)
esDecisiva :: Actividad -> Logro -> Personaje -> Bool
esDecisiva unaActividad unLogro unPersonaje = (not.unLogro) unPersonaje && (unLogro.unaActividad) unPersonaje 

-- B)
primerActividadDecisiva :: Personaje -> Logro -> [Actividad] -> Personaje
primerActividadDecisiva unaPersona _ [] = unaPersona
primerActividadDecisiva unaPersona unLogro (x:xs) 
    | esDecisiva x unLogro unaPersona = x unaPersona
    | otherwise = primerActividadDecisiva unaPersona unLogro xs

-- C)
actividadesInfinitas :: [Actividad]
actividadesInfinitas = repeat (comerNDonas 1)