import Text.Show.Functions
data Investigador = Investigador {
    nombre :: String,
    cordura :: Int,
    items :: [Item],
    sucesosEvitados :: [String]
} deriving (Show, Eq)

data Item = Item {
    nombreItem :: String,
    valor :: Int
} deriving (Show, Eq)

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
    | f a > f b = a
    | otherwise = b

deltaSegun :: Num a => (b -> a) -> (b -> b) -> b -> a
deltaSegun ponderacion transformacion valor = abs ((ponderacion.transformacion) valor - ponderacion valor)

-- PUNTO 1
-- A)
enloquecer :: Int -> Investigador -> Investigador
enloquecer cantidad unInvestigador 
    | cordura unInvestigador - cantidad >= 0 = unInvestigador {cordura = cordura unInvestigador - cantidad}
    | otherwise = unInvestigador {cordura = 0}

-- B)
hallar :: Item -> Investigador -> Investigador
hallar unItem  = enloquecer (valor unItem).agregarItem unItem 

agregarItem :: Item -> Investigador -> Investigador
agregarItem unItem unInvestigador = unInvestigador {items = unItem : items unInvestigador}

-- PUNTO 2
algunoTieneElItem :: String -> [Investigador] -> Bool
algunoTieneElItem unItem = any (elem unItem. map nombreItem. items) 

-- PUNTO 3
elLider :: [Investigador] -> Investigador
elLider = maximoSegun potencial 

potencial :: Investigador -> Int
potencial unInvestigador 
    | estaLoco unInvestigador = 0
    | otherwise               = cordura unInvestigador * experiencia unInvestigador + (valor.itemDeMaximoValor) unInvestigador

experiencia :: Investigador -> Int
experiencia unInvestigador = 1 + (length.sucesosEvitados) unInvestigador 

estaLoco :: Investigador -> Bool
estaLoco unInvestigador = cordura unInvestigador == 0

itemDeMaximoValor :: Investigador -> Item
itemDeMaximoValor unInvestigador = maximoSegun valor (items unInvestigador)

-- PUNTO 4
-- A)
deltaEnCorduraTotal :: Int -> [Investigador] -> Int
deltaEnCorduraTotal unaCantidad =sum . map (deltaSegun cordura (enloquecer unaCantidad)) 

-- B)
deltaEnPotencial :: [Investigador] -> Int
deltaEnPotencial = deltaSegun (head.map potencial) (filter (not.estaLoco))

-- PUNTO 5
data Suceso = Suceso {
    descripcion :: String,
    consecuencias :: [Consecuencia],
    formaDeEvitarlo :: Forma
} deriving (Show)

type Consecuencia = [Investigador] -> [Investigador] 
type Forma = [Investigador] -> Bool

despertarDeUnAntiguo :: Suceso
despertarDeUnAntiguo = Suceso {
    descripcion = "Despertar de un antiguo",
    consecuencias = [map (enloquecer 10), tail],
    formaDeEvitarlo = algunoTieneElItem "Necronomicon"
}

ritualEnInnsmouth :: Suceso
ritualEnInnsmouth = Suceso {
    descripcion = "Ritual en Innsmouth",
    consecuencias = [(:[]).hallar dagaMaldita .head, enfrentar despertarDeUnAntiguo],
    formaDeEvitarlo = (>100).potencial.head
}

dagaMaldita :: Item
dagaMaldita = Item {
    nombreItem = "Daga Maldita",
    valor = 3
}
-- PUNTO 6
enfrentar :: Suceso -> [Investigador] -> [Investigador]
enfrentar unSuceso losInvestigadores
    | formaDeEvitarlo unSuceso (map (enloquecer 10)losInvestigadores) = map (enloquecer 10.incorporarSuceso (descripcion unSuceso)) losInvestigadores
    | otherwise = map (enloquecer 10). sufrirConsecuencias unSuceso $ losInvestigadores 

incorporarSuceso :: String -> Investigador -> Investigador
incorporarSuceso unSuceso unInvestigador = unInvestigador {sucesosEvitados = unSuceso : sucesosEvitados unInvestigador}

sufrirConsecuencias :: Suceso -> [Investigador] -> [Investigador]
sufrirConsecuencias unSuceso losInvestigadores = foldl (\investigadores unaConsecuencia -> unaConsecuencia investigadores) losInvestigadores (consecuencias unSuceso)

-- PUNTO 7
elMasAterrador :: Int -> [Suceso] -> [Investigador] -> Suceso
elMasAterrador _ [] _ = undefined
elMasAterrador _ [unSoloSuceso] _ = unSoloSuceso
elMasAterrador cantidad (x:y:xs) losInvestigadores
    | (deltaEnCorduraTotal cantidad. enfrentar x) losInvestigadores > (deltaEnCorduraTotal cantidad. enfrentar y) losInvestigadores = elMasAterrador cantidad (x:xs) losInvestigadores
    | otherwise = elMasAterrador cantidad (y:xs) losInvestigadores