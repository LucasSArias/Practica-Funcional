-- PARTE A
data Participante = Participante {
    nombre :: String,
    trucos :: [Truco],
    especialidad :: Plato
}

type Truco = Plato -> Plato

data Plato = Plato {
    dificultad :: Int,
    componentes :: [Ingrediente]
}

type Ingrediente = (Nombre, Peso) 
type Nombre = String
type Peso = Int

-- 1)
endulzar :: Int ->  Truco
endulzar cantidad = agregar ("azucar", cantidad) 

agregar :: Ingrediente -> Plato -> Plato
agregar unIngrediente unPlato = unPlato {componentes = unIngrediente : componentes unPlato}

-- 2)
salar :: Int -> Truco
salar cantidad = agregar ("sal", cantidad)

-- 3)
darSabor :: Int -> Int -> Truco
darSabor cantSal cantAzucar = agregar ("azucar", cantAzucar). agregar ("sal", cantSal) 

-- 4) 
duplicarPorcion :: Truco
duplicarPorcion unPlato = unPlato {componentes = map duplicarIngrediente (componentes unPlato)}

duplicarIngrediente :: Ingrediente -> Ingrediente
duplicarIngrediente unIngrediente = (fst unIngrediente, (2*).snd $ unIngrediente)

-- 5)
simplificar :: Truco
simplificar unPlato 
    | esComplejo unPlato = unPlato {dificultad = 5, componentes = filter ((<10).snd) (componentes unPlato)} 
    | otherwise = unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = ((>5).length.componentes) unPlato && ((>7).dificultad) unPlato

esVegano :: Plato -> Bool
esVegano unPlato = (not.tiene "carne") unPlato && (not.tiene "huevo") unPlato && (not.tiene "leche") unPlato

tiene :: String -> Plato -> Bool
tiene unIngrediente unPlato = elem unIngrediente (map fst.componentes $ unPlato)

esSinTacc :: Plato -> Bool
esSinTacc = not.tiene "harina"

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = tiene "sal" unPlato && ((>2).cantidadDe "sal") unPlato  

cantidadDe :: String -> Plato -> Int
cantidadDe unIngrediente unPlato = snd.head.filter ((==unIngrediente).fst).componentes $ unPlato

-- PARTE B
pepe :: Participante 
pepe = Participante {
    nombre = "Pepe Ronccino",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    especialidad = platoDePepe
}

platoDePepe :: Plato
platoDePepe = Plato 10 (repeat ("sal", 1))

-- PARTE C

cocinar :: Participante -> Plato
cocinar unParticipante = foldl (\unPlato unTruco -> unTruco unPlato) (especialidad unParticipante) (trucos unParticipante)

esMejorQue :: Plato -> Plato -> Bool
esMejorQue elMejor elPeor = dificultad elMejor > dificultad elPeor && sumaPesos elMejor < sumaPesos elPeor

sumaPesos :: Plato -> Int
sumaPesos = sum. map snd .componentes 

participanteEstrella :: [Participante] -> Participante
participanteEstrella [] = undefined
participanteEstrella [unSoloParticipante] = unSoloParticipante
participanteEstrella (x:y:xs)
    | cocinar x `esMejorQue` cocinar y   = participanteEstrella (x:xs)
    | otherwise                          = participanteEstrella (y:xs)

-- PARTE D
platinum :: Plato
platinum = Plato {
    dificultad = 10,
    componentes = infinitosComponentes
}

infinitosComponentes :: [Ingrediente]
infinitosComponentes = zip infinitosIngredientes [1..]

infinitosIngredientes :: [String]
infinitosIngredientes = map (("Ingrediente "++).show) [1..]

-- Las funciones endulzar, salar y darSabor van a funcionar con normalidad, ya que añaden el ingrediente correspondiente a la cabeza de la lista de ingredientes. Mientras que duplicarPorcion y simplificar son funciones que implicar recorrer la totalidad de la lista de componentes para poder aplicar la moodificación al plato, y como la cantidad de componentes es infinita, estas funciones se quedan calculando el plato por tiempo indeterminado.

-- Solo se puede responder esComplejo, el resto de condiciones implican recorrer la lista de componentes en su totalidad para llegar a un resultado y esto no es posible dado que hay infinitos componentes.

-- No se puede saber si platinum es mejor que otro plato debido a que su dificultad si es un valor acotado, pero la suma de los pesos de sus ingredientes es indeterminada y por lo tanto no puede ser comparable con ningun plato.