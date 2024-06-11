-- PARTE A
data Perrito = Perrito {
    raza :: String,
    juguetesFavoritos :: [Juguete],
    tiempoAPermanecer :: Int,
    energia :: Int
}

type Juguete = String

data Guarderia = Guarderia {
    nombre :: String,
    rutina :: [(Actividad, Tiempo)]
}

type Actividad = Perrito -> Perrito
type Tiempo = Int

jugar :: Actividad
jugar unPerrito
    | (energia.modificarEnergia (flip (-) 10)) unPerrito > 0 = modificarEnergia (flip (-) 10) unPerrito
    | otherwise = modificarEnergia (flip (-) (energia unPerrito)) unPerrito

modificarEnergia :: (Int -> Int) -> Perrito -> Perrito
modificarEnergia operacion unPerrito = unPerrito {energia = operacion.energia $ unPerrito}

ladrar :: Int ->  Actividad
ladrar cantidad = modificarEnergia (+ div cantidad 2) 

regalar :: Juguete -> Actividad
regalar unJuguete unPerrito = unPerrito {juguetesFavoritos = unJuguete : juguetesFavoritos unPerrito}

diaDeSpa :: Actividad
diaDeSpa unPerrito 
    | tiempoAPermanecer unPerrito > 50 && esRazaExtravagante unPerrito = regalar "peine de goma".energiaAlPalo $ unPerrito
    | otherwise = unPerrito

esRazaExtravagante :: Perrito -> Bool
esRazaExtravagante unPerrito = elem (raza unPerrito) ["dalmata", "pomerania"]

energiaAlPalo :: Perrito -> Perrito
energiaAlPalo unPerrito = unPerrito {energia = 100}

diaDeCampo :: Actividad
diaDeCampo = pierdePrimerJuguete.jugar 

pierdePrimerJuguete :: Perrito -> Perrito
pierdePrimerJuguete unPerrito = unPerrito {juguetesFavoritos = tail.juguetesFavoritos $ unPerrito}


zara :: Perrito
zara = Perrito {
    raza = "dalmata",
    juguetesFavoritos = ["pelota", "mantita"],
    tiempoAPermanecer = 90,
    energia = 80
}

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = Guarderia {
    nombre = "Guarderia P de Perritos",
    rutina = [(jugar, 30), (ladrar 18, 20), (regalar "pelota", 0), (diaDeSpa, 120), (diaDeCampo, 720)]
}
-- PARTE B
tiempoRutina :: Guarderia -> Int
tiempoRutina = sum . map snd. rutina

puedeEstarEn :: Guarderia -> Perrito -> Bool
puedeEstarEn unaGuarderia unPerrito = tiempoAPermanecer unPerrito > tiempoRutina unaGuarderia

esResponsable :: Perrito -> Bool
esResponsable = ( > 3).length.juguetesFavoritos.diaDeCampo 

realizarRutina :: Guarderia -> Perrito -> Perrito
realizarRutina unaGuarderia unPerrito
    | puedeEstarEn unaGuarderia unPerrito = foldl (\perro actividad -> actividad perro) unPerrito (map fst.rutina $ unaGuarderia)
    | otherwise = unPerrito

estaCansado :: Perrito -> Bool
estaCansado = (<5).energia

quedanCansados :: Guarderia -> [Perrito] -> [Perrito]
quedanCansados unaGuarderia = filter estaCansado .map (realizarRutina unaGuarderia) 

-- PARTE C
piPerrito :: Perrito
piPerrito = Perrito {
    raza = "labrador",
    juguetesFavoritos = infinitasSogas,
    tiempoAPermanecer = 314,
    energia = 159
}

infinitasSogas :: [Juguete]
infinitasSogas = map (("soga "++). show)  [1..]

{-
1) Es posible averiguar la raza de pi, entonces se puede determinar si es extravagante al pasar a pi como argumento de la funcion esRazaExtravagante, que dara como resultado False
2)  a) habría que recorrer la interminable cantidad de sogas para despues ver si hay algun huesito, pero como la cantidad de sogas no tiene fin, nunca se determinaria si un huesito forma parte de su lista de juguetes.
    b) Si se encontraría ya que los juguetes agregados por el diaDeSpa (que es una actividad que forma parte de la rutina Guarderia de Perritos) encabeza a la pelota dentro de la lista de juguetes, y gracias a la evaluación diferida de haskell, cuando ya se encuentra al juguete buscado, se deja de mirar al resto de juguetes y se salta a la respuesta, que devolvería True.
    c) Idem respuesta b
3) Es posible, mientras que las actividades de la rutina no involucren procesos con su lista de juguetes que no lleguen a una respuesta en una cantidad finita de tiempo.
4) Se va a encabezar en su lista de juguetes, sus juguetes favoritos quedarian de la forma: ["hueso", "soga 1", "soga 2",...
-}