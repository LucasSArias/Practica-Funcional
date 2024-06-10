import Text.Show.Functions
import qualified Main as robaDestornillador
data Plomero = Plomero {
    nombre :: String,
    cajaDeHerramientas :: [Herramienta],
    historialReparaciones :: [Reparacion], -- el tipo Reparacion queda definido mas adelante en el examen
    dinero :: Float
} deriving Show

data Herramienta = Herramienta {
    nombreHerramienta :: String,
    precio :: Float,
    materialEmpuñadura :: Material
} deriving (Eq, Show)

data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)

-- PUNTO 1
-- A) 
mario :: Plomero
mario = Plomero {
    nombre = "Mario",
    dinero = 1200,
    historialReparaciones = [],
    cajaDeHerramientas = [llaveInglesa, martillo Madera]
}

llaveInglesa :: Herramienta
llaveInglesa = Herramienta {
    nombreHerramienta = "Llave inglesa",
    precio = 200,
    materialEmpuñadura = Hierro
}

martillo :: Material -> Herramienta
martillo unMaterial = Herramienta "Martillo" 20 unMaterial

-- B)
wario :: Plomero
wario = Plomero {
    nombre = "Wario",
    dinero = 0.5,
    historialReparaciones = [],
    cajaDeHerramientas = infinitasLLavesFrancesas
}

llaveFrancesa :: Herramienta
llaveFrancesa = Herramienta {
    nombreHerramienta = "Llave francesa",
    precio = 1,
    materialEmpuñadura = Hierro
}

subirPrecio :: Int -> Herramienta -> Herramienta
subirPrecio cantidad unaHerramienta = unaHerramienta {precio = cantidad + precio unaHerramienta}

infinitasLLavesFrancesas :: [Herramienta]
infinitasLLavesFrancesas = llaveFrancesa : subsiguientesLlavesFrancesas llaveFrancesa

subsiguientesLlavesFrancesas :: Herramienta -> [Herramienta]
subsiguientesLlavesFrancesas unaLlaveFrancesa = subirPrecio 1 unaLlaveFrancesa : subsiguientesLlavesFrancesas (subirPrecio 1 unaLlaveFrancesa)

-- PUNTO 2
-- A)
tiene :: String -> Plomero -> Bool
tiene unaHerramienta = elem unaHerramienta . map nombreHerramienta . cajaDeHerramientas

-- B)
esMalvado :: Plomero -> Bool
esMalvado = (== "Wa") . take 2 . nombre

-- C)
puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar unaHerramienta unPlomero = dinero unPlomero > precio unaHerramienta

-- PUNTO 3
esBuena :: Herramienta -> Bool
esBuena unaHerramienta = (((== Hierro).materialEmpuñadura) unaHerramienta  && ((> 10000).precio) unaHerramienta ) || unaHerramienta == martillo Madera || unaHerramienta == martillo Goma

-- PUNTO 4
comprar :: Herramienta -> Plomero -> Plomero
comprar unaHerramienta unPlomero 
    | puedeComprar unaHerramienta unPlomero = agregarHerramienta unaHerramienta . pagarPrecio (precio unaHerramienta) $ unPlomero
    | otherwise = unPlomero

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta unaHerramienta unPlomero = unPlomero {cajaDeHerramientas = unaHerramienta : cajaDeHerramientas unPlomero}

pagarPrecio :: Float -> Plomero -> Plomero
pagarPrecio cantidad unPlomero = unPlomero {dinero = dinero unPlomero - cantidad}

-- PUNTO 5
type Reparacion = (Descripcion, Requerimiento)
type Descripcion = String
type Requerimiento = Plomero -> Bool

-- A)
filtracionDeAgua :: Reparacion
filtracionDeAgua = ("Filtracion de agua", tiene "Llave inglesa")

-- B) 
esDificil :: Reparacion -> Bool
esDificil unaReparacion = ((>100).length.fst) unaReparacion && (all isUpper.fst) unaReparacion

-- C)
presupuesto :: Reparacion -> Float
presupuesto = (3*).length.fst 

-- PUNTO 6
hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion unaReparacion unPlomero 
    | esMalvado unPlomero && tiene "martillo" unPlomero = robaDestornillador.cobrarReparacion unaReparacion $ unPlomero 
    | (snd unaReparacion unPlomero || (esMalvado unPlomero && tiene "martillo" unPlomero )) && (not.esMalvado) unPlomero && esDificil unaReparacion       = pierdeTodaslasHerramientasBuenas.(cobrarReparacion unaReparacion) $ unPlomero
    | (snd unaReparacion unPlomero || (esMalvado unPlomero && tiene "martillo" unPlomero )) && (not.esMalvado) unPlomero && (not.esDificil) unaReparacion = pierdeLaPrimerHerramienta.(cobrarReparacion unaReparacion) $ unPlomero                                  
    | otherwise = unPlomero {dinero = dinero unPlomero + 100}



cobrarReparacion :: Reparacion -> Plomero -> Plomero
cobrarReparacion unaReparacion unPlomero = unPlomero {dinero = dinero unPlomero + presupuesto unaReparacion}

pierdeLaPrimerHerramienta :: Plomero -> Plomero
pierdeLaPrimerHerramienta unPlomero = unPlomero {cajaDeHerramientas = tail.cajaDeHerramientas $ unPlomero}

pierdeTodaslasHerramientasBuenas :: Plomero -> Plomero
pierdeTodaslasHerramientasBuenas unPlomero = unPlomero {cajaDeHerramientas = filter (not.esBuena) (cajaDeHerramientas unPlomero) }

-- PUNTO 7
jornadaLaboral :: [Reparacion] -> Plomero -> Plomero
jornadaLaboral reparaciones unPlomero = foldl (flip hacerReparacion) unPlomero reparaciones

-- PUNTO 8
mas :: (Plomero -> Float) -> [Plomero] -> Plomero
mas _ [] = undefined
mas _ [unSoloPlomero] = unSoloPlomero
mas parametro (x:y:xs) 
    | parametro x > parametro y = mas parametro (x:xs)
    | otherwise                 = mas parametro (y:xs)

mayorParametroTrasJornada :: ([Plomero] -> Plomero) -> [Reparacion] -> [Plomero] -> Plomero
mayorParametroTrasJornada parametro lasReparaciones losPlomeros = parametro . map (jornadaLaboral lasReparaciones) $ losPlomeros 

-- A)
masReparador :: [Plomero] -> Plomero
masReparador = mas (length .historialReparaciones) -- SI LABURABA CON NUMBERS ERA MENOS QUILOMBO

masReparadorTrasJornada :: [Reparacion] -> [Plomero] -> Plomero
masReparadorTrasJornada = mayorParametroTrasJornada masReparador

-- B)
masAdinerado :: [Plomero] -> Plomero
masAdinerado = mas dinero

masAdineradoTrasJornada :: [Reparacion] -> [Plomero] -> Plomero
masAdineradoTrasJornada  = mayorParametroTrasJornada masAdinerado

-- C)
masPlataInvertida :: [Plomero] -> Plomero
masPlataInvertida = mas (sum . map precio . cajaDeHerramientas)

masPlataInvertidaTrasJornada :: [Reparacion] -> [Plomero] -> Plomero
masPlataInvertidaTrasJornada = mayorParametroTrasJornada masPlataInvertida

