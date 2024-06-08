-- PRIMERA PARTE
-- PUNTO 1
type Criatura = (Peligrosidad, Condicion)
type Peligrosidad = Int
type Condicion = Persona -> Bool

data Persona = Persona {
    edad :: Int,
    items :: [Item],
    experiencia :: Int
}

type Item = String

siempredetras :: Criatura
siempredetras = (0, noDejaEnPaz)

noDejaEnPaz :: Condicion
noDejaEnPaz unaPersona = True

gnomos :: Int -> Criatura
gnomos cantidad = (2 ^ cantidad, tiene "soplador de hojas")

tiene :: Item -> Condicion
tiene unItem unaPersona = elem unItem (items unaPersona)

fantasma :: Int -> Condicion -> Criatura
fantasma categoria condicion = (20 * categoria, condicion) 

--PUNTO 2
enfrentar :: Persona -> Criatura -> Persona
enfrentar unaPersona unaCriatura 
    | snd unaCriatura unaPersona = agregarExperiencia (fst unaCriatura) unaPersona
    | otherwise = agregarExperiencia 1 unaPersona

agregarExperiencia :: Int -> Persona -> Persona
agregarExperiencia cantidad unaPersona = unaPersona {experiencia = experiencia unaPersona + cantidad}

--PUNTO 3
-- a)
experienciaGanada :: Persona -> [Criatura] -> Int
experienciaGanada unaPersona criaturas = (experiencia . foldl enfrentar unaPersona) criaturas - experiencia unaPersona
-- b)
grupoCriaturas :: [Criatura]
grupoCriaturas = [siempredetras, gnomos 10, fantasma 3 condicionFantasma3, fantasma 1 ((>10).experiencia)]

condicionFantasma3 :: Condicion
condicionFantasma3 unaPersona = tiene "disfraz de oveja" unaPersona && edad unaPersona < 13

-- SEGUNDA PARTE
-- PUNTO 1
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ [] listaB = listaB
zipWithIf _ _ _ [] = []
zipWithIf operacion condicion (a:as) (b:bs) 
    | condicion b = operacion a b : zipWithIf operacion condicion as bs 
    | otherwise = b : zipWithIf operacion condicion (a:as) bs

-- PUNTO 2
-- a)
abecedarioDesde :: Char -> [Char]
abecedarioDesde unaLetra = init ([unaLetra .. 'z'] ++ ['a' .. unaLetra])

-- b)
desencriptarLetra :: Char -> Char -> Char
desencriptarLetra clave unaLetra = 

