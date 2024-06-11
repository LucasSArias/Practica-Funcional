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
