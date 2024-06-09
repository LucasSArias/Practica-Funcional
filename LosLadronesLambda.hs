-- PARTE 1
-- PUNTO A
type Tesoro = (AnioDescubrimiento, Precio)
type AnioDescubrimiento = Int
type Precio = Int

esDeLujo :: Int -> Tesoro -> Bool
esDeLujo anioActual unTesoro = snd unTesoro > 1000 || antiguedad anioActual unTesoro > 200

antiguedad :: Int -> Tesoro -> Int
antiguedad anioActual unTesoro = anioActual - snd unTesoro

esDeTelaSucia :: Int -> Tesoro -> Bool
esDeTelaSucia anioActual unTesoro = valor anioActual unTesoro < 50 && (not.esDeLujo anioActual) unTesoro

esEstandar :: Int -> Tesoro -> Bool
esEstandar anioActual unTesoro = (not.esDeTelaSucia anioActual) unTesoro && (not.esDeLujo anioActual) unTesoro

-- PUNTO B
valor :: Int -> Tesoro -> Int
valor anioActual unTesoro = snd unTesoro + 2 * antiguedad anioActual unTesoro

-- PARTE 2
data Cerradura = Cerradura {clave :: Clave}
type Clave = [Char]

type Herramienta = Cerradura -> Cerradura

martillo :: Herramienta
martillo unaCerradura = unaCerradura{clave = (drop 3 . clave) unaCerradura} 

llaveMaestra :: Herramienta
llaveMaestra unaCerradura = unaCerradura {clave = []}

ganzua :: (Clave -> Clave) -> Herramienta
ganzua operacion unaCerradura = unaCerradura {clave = (operacion.clave) unaCerradura}

gancho :: Clave -> Clave
gancho = filter (not.isUpper) 

rastrillo :: Clave -> Clave
rastrillo unaClave = filter (not.isDigit)

--rombo :: String -> Clave -> Clave               -- VER COMO HACER ESTA FUNCION
--rombo inscripcion unaClave =    

tensor :: Herramienta
tensor unaCerradura = unaCerradura {clave = map toUpper (clave unaCerradura)}

socotroco :: Herramienta -> Herramienta -> Herramienta
socotroco unaHerramienta otraHerramienta = otraHerramienta.unaHerramienta

-- PARTE 3
data Ladron = Ladron {
    nombre :: String,
    herramientas :: [Herramienta],
    tesoros :: [Tesoro]
}

data Cofre = Cofre {
    cerradura :: Cerradura,
    tesoro :: Tesoro
}

-- PUNTO A
experiencia :: Int -> Ladron -> Int
experiencia anioActual = sum.map (valor anioActual).tesoros 

esLegendario :: Int -> Ladron -> Bool
esLegendario anioActual unLadron = experiencia anioActual unLadron > 100 && all (esDeLujo anioActual) (tesoros unLadron) 

-- PUNTO B
robar :: Ladron -> Cofre -> Ladron
robar unLadron unCofre 
    | noTieneHerramientas unLadron && estaAbierto unCofre = agregoTesoro (tesoro unCofre) unLadron
    | noTieneHerramientas unLadron = unLadron
    | estaAbierto unCofre = agregoTesoro (tesoro unCofre) unLadron
    | otherwise = robar (conUnaHerramientaMenos unLadron) (debilitarCerradura ((head.herramientas) unLadron) unCofre)

debilitarCerradura :: Herramienta -> Cofre -> Cofre
debilitarCerradura unaHerramienta unCofre = unCofre {cerradura = unaHerramienta (cerradura unCofre)}

conUnaHerramientaMenos :: Ladron -> Ladron
conUnaHerramientaMenos unLadron = unLadron {herramientas = tail.herramientas $ unLadron}

estaAbierto :: Cofre -> Bool
estaAbierto = null.clave.cerradura

noTieneHerramientas :: Ladron -> Bool
noTieneHerramientas = null.herramientas

agregoTesoro :: Tesoro -> Ladron -> Ladron
agregoTesoro unTesoro unLadron = unLadron {tesoros = unTesoro : tesoros unLadron}

-- PUNTO C
atraco :: Ladron -> [Cofre] -> Ladron
atraco = foldl robar 

-- PUNTO D
