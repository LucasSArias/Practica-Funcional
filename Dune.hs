-- PUNTO 1
data Fremen = Fremen {
    nombre :: String,
    nivelDeTolerancia :: Float,
    titulos :: [Titulo],
    cantidadReconocimientos :: Int
} deriving (Show, Eq)

type Titulo = String
type Tribu  = [Fremen]

stilgar :: Fremen
stilgar = Fremen "Stilgar" 150 ["Guia"] 3

-- 1)a)
nuevoReconocimiento :: Titulo -> Fremen -> Fremen
nuevoReconocimiento unTitulo unFremen = unFremen {titulos = unTitulo : titulos unFremen}
-- 1)b)
hayCandidatosAElegido :: Tribu -> Bool
hayCandidatosAElegido = any (toleranciaMayorA 100) . filter esDomador  

esDomador :: Fremen -> Bool
esDomador  = elem "Domador".titulos

toleranciaMayorA :: Float -> Fremen -> Bool
toleranciaMayorA n unFremen = n < nivelDeTolerancia unFremen
-- 1)c)
elElegido :: Tribu -> Fremen
elElegido [] = undefined                -- esta linea no hace falta, haskell me hinchaba las bolas con que el pattern matching no era exhaustivo 
elElegido [unFremen] = unFremen
elElegido (x:xs)     = foldl mayorReconocimiento x xs

mayorReconocimiento :: Fremen -> Fremen -> Fremen
mayorReconocimiento superior inferior  
    | cantidadReconocimientos superior > cantidadReconocimientos inferior = superior
    | otherwise = inferior 

-- PUNTO 2
data Gusano = Gusano {
    longitud :: Float,
    nivelHidratacion :: Int,
    descripcion :: String
} deriving Eq

reproducir :: Gusano -> Gusano -> Gusano
reproducir padre madre = Gusano {
    longitud = (0.1*) $ max (longitud padre) (longitud madre),
    nivelHidratacion = 0,
    descripcion = descripcion padre ++ descripcion madre
}

crias :: [Gusano] -> [Gusano] -> [Gusano]
crias _ [] = []
crias [] _ = []
crias (x1:xs1) (x2:xs2) = reproducir x1 x2 : crias xs1 xs2

-- PUNTO 3
type Mision = Gusano -> Fremen -> Fremen

domarGusano :: Mision
domarGusano unGusano unFremen  
    | nivelDeTolerancia unFremen >= longitud unGusano / 2 = otorgaTitulo "Domador".modificaTolerancia (+) 100 $ unFremen
    | otherwise = modificaTolerancia (-) (0.1 * nivelDeTolerancia unFremen) unFremen 

destruirGusano :: Titulo -> Mision
destruirGusano unReconocimiento unGusano unFremen
    | esDomador unFremen && nivelDeTolerancia unFremen < longitud unGusano / 2 = otorgaTitulo unReconocimiento. modificaTolerancia (+) 100 $ unFremen
    | otherwise = modificaTolerancia (-) (0.2 * nivelDeTolerancia unFremen) unFremen

otorgaTitulo :: Titulo -> Fremen -> Fremen
otorgaTitulo unTitulo unFremen = unFremen {titulos = unTitulo : titulos unFremen}

modificaTolerancia :: (Float -> Float -> Float) -> Float -> Fremen -> Fremen
modificaTolerancia operacion cantidad unFremen = unFremen {nivelDeTolerancia = operacion (nivelDeTolerancia unFremen) cantidad}

realizacionColectiva :: Tribu -> Mision -> Gusano -> Tribu
realizacionColectiva unaTribu unaMision unGusano = map (unaMision unGusano) unaTribu

cambioElElegido :: Tribu -> Mision -> Gusano -> Bool
cambioElElegido unaTribu unaMision unGusano = elElegido unaTribu /= (elElegido.realizacionColectiva unaTribu unaMision) unGusano

-- PUNTO 4
{-
a) No se que se fumó el enunciado
b) La función retornaría True con que exista al menos 1 candidato a ser elegido, debido a la evaluación diferida de haskell, gracias a la cual no es necesario recorrer la totalidad de la lista infinita para asegurar la existencia de minimo 1 candidato.
c) La funcion elElegido nunca va a otorgar una respuesta dado que debe buscar el Fremen de máximo reconocimiento entre TODOS los infinitos Fremens. Como elElegido debe llegar al final de una lista para retornar al fremen elegido, y esta lista resulta no tener fin, la función nunca va a devolver una respuesta ya que se "cuelga" tomando los infinitos fremens de la lista.
-}