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
algunoTieneElItem :: Item -> [Investigador] -> Bool
algunoTieneElItem unItem = any (elem unItem. items) 

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
}

type Consecuencia = [Investigador] -> [Investigador]
type Forma = [Investigador] -> Bool
