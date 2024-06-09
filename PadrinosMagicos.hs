import Text.Show.Functions
data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
}   deriving (Show)

type Habilidad = String

-- CONCEDIENDO DESEOS
type Deseo = Chico -> Chico 
-- A)
aprenderHabilidades :: [Habilidad] -> Chico -> Chico
aprenderHabilidades nuevasHabilidades unChico = unChico {habilidades = nuevasHabilidades ++ habilidades unChico}

-- B)
serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = unChico {habilidades = habilidades unChico ++ todosLosNFS}

todosLosNFS :: [Habilidad]
todosLosNFS = map (("jugar Need for Speed "++) . show) [1..]

-- C)
serMayor :: Deseo
serMayor = cambiarEdad 18 

cambiarEdad :: Int -> Chico -> Chico
cambiarEdad nuevaEdad unChico = unChico {edad = nuevaEdad}

-- PUNTO 2
-- A)
type Padrino = Chico -> Chico
wanda :: Padrino
wanda unChico = cambiarEdad (edad unChico +1).(head.deseos) unChico $ unChico

cosmo :: Padrino
cosmo unChico = cambiarEdad (flip div 2 . edad $ unChico) unChico

muffinMagico :: Chico -> Chico
muffinMagico unChico = foldl (\elChico unDeseo -> unDeseo elChico) unChico (deseos unChico)   

-- EN BUSCA DE PAREJA
data Chica = Chica {
    nombreChica :: String,
    condicion :: Condicion
} deriving Show

type Condicion = Chico -> Bool
-- PUNTO 1
-- A)
tieneHabilidad :: String -> Condicion
tieneHabilidad unaHabilidad unChico = elem unaHabilidad (habilidades unChico)
-- B)
esSuperMaduro :: Condicion
esSuperMaduro unChico = edad unChico > 18 && tieneHabilidad "manejar" unChico

-- PUNTO 2
-- A)
quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA _ []            = undefined
quienConquistaA _ [unSoloChico] = unSoloChico
quienConquistaA unaChica (x:xs)
    | condicion unaChica x  = x
    | otherwise             = quienConquistaA unaChica xs

-- B)
delfi :: Chica
delfi = Chica {
    nombreChica = "Delfi",
    condicion = tieneHabilidad "sabe cocinar"
}

lucas :: Chico 
lucas = Chico {
    nombre = "Lucas",
    edad = 20,
    habilidades = ["sabe cocinar"],
    deseos = []
}

pollo :: Chico
pollo = Chico {
    nombre = "Maria Florencia Pollo Cataneo",
    edad = 50,
    habilidades = ["sabe cocinar"],
    deseos = []
}

-- DA RULES

-- PUNTO 1
infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules = map nombre. filter infringeDaRules

infringeDaRules :: Chico -> Bool
infringeDaRules unChico = any (esDeseoProhibido unChico) (deseos unChico)

esDeseoProhibido :: Chico -> Deseo -> Bool
esDeseoProhibido unChico unDeseo = elem habilidadesProhibidas (habilidades (unDeseo unChico))

habilidadesProhibidas :: [Habilidad]
habilidadesProhibidas = ["enamorar","matar","dominar el mundo"]

habilidadesDespuesDeDeseo :: Deseo -> Chico -> [Habilidad]
habilidadesDespuesDeDeseo unDeseo = habilidades. unDeseo 