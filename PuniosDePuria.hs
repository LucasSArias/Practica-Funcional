-- PUNTO 1
-- A)
data Peleador = Peleador {
    vida :: Int,
    resistencia :: Resistencia,
    ataques :: [Ataque]
}
type Resistencia = Int
type Ataque = Peleador -> Peleador
-- B)
estaMuerto :: Peleador -> Bool
estaMuerto unPeleador = vida unPeleador < 1

esHabil :: Peleador -> Bool
esHabil unPeleador = (length.ataques) unPeleador > 10

-- C)
golpe :: Int -> Ataque
golpe intensidad unOponente = reduceVidaEn (div intensidad. resistencia $ unOponente) unOponente

reduceVidaEn :: Int -> Peleador -> Peleador
reduceVidaEn cantidad unPeleador = unPeleador {vida = vida unPeleador - cantidad}

toqueDeLaMuerte :: Ataque
toqueDeLaMuerte unOponente = reduceVidaEn (vida unOponente) unOponente

patada :: String -> Ataque
patada "el pecho" unOponente 
    | estaMuerto unOponente = unOponente {vida = 1}
    | otherwise             = reduceVidaEn 10 unOponente
patada "la carita" unOponente = reduceVidaEn (flip div 2.vida $ unOponente) unOponente
patada "la nuca" unOponente = unOponente {ataques = tail.ataques $ unOponente}
patada _ unOponente = unOponente

-- D)
bruceLee :: Peleador
bruceLee = Peleador {
    vida = 200,
    resistencia = 25,
    ataques = [toqueDeLaMuerte, golpe 500, patada "la carita".patada "la carita".patada "la carita"]
}

-- PUNTO 2
-- VOY A USAR RECURSIVIDAD EN NINGUN LADO DICE QUE ESTÁ PROHIBIDO MASTIQUENME LAS BOLAS 😁 
mejorAtaque :: Peleador -> Peleador -> Ataque
mejorAtaque (Peleador _ _ []) _ = undefined
mejorAtaque (Peleador _ _ [unSoloAtaque]) _ = unSoloAtaque
mejorAtaque (Peleador _ _ (x:y:xs)) unEnemigo 
    | (vida.x $ unEnemigo) < (vida.y $ unEnemigo) = mejorAtaque (Peleador 0 0 (x:xs)) unEnemigo
    | otherwise = mejorAtaque (Peleador 0 0 (y:xs)) unEnemigo

-- PUNTO 3
-- A)
terrible :: [Peleador] -> Ataque ->  Bool
terrible enemigos unAtaque = (flip div 2 . length $ enemigos) > (length.filter estaMuerto. map unAtaque $ enemigos) 

peligroso :: [Peleador] -> Peleador -> Bool
peligroso enemigos unPeleador = all (terrible.filter esHabil $ enemigos) (ataques unPeleador)

invencible :: [Peleador] -> Peleador -> Bool
invencible enemigos unPeleador = vida unPeleador == vida (foldl (\elPeleador unAtaque -> unAtaque elPeleador) unPeleador (map (mejorAtaque unPeleador) enemigos))