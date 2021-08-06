-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = UnTiro {
    velocidad = 10,
    precision = (precisionJugador unaHabilidad) * 2,
    altura = 0
}

madera :: Palo
madera unaHabilidad = UnTiro {
    velocidad = 100,
    altura = 5,
    precision = div (precisionJugador unaHabilidad) 2
}

hierro :: Int -> Palo
hierro n unaHabilidad = UnTiro {
    velocidad = (fuerzaJugador unaHabilidad) * n,
    altura = max 0 (n - 3),
    precision = div (precisionJugador unaHabilidad) n
}

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

-- PUNTO 2
golpe :: Jugador -> Palo -> Tiro
golpe unJugador unPalo = unPalo . habilidad $ unJugador

-- PUNTO 3
data Obstaculo = Obstaculo {
    puedeSuperarlo :: Tiro -> Bool,
    efecto         :: Tiro -> Tiro
}

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo unObstaculo unTiro
 | (puedeSuperarlo unObstaculo) unTiro = (efecto unObstaculo) unTiro
 | otherwise                           = tiroDetenido


tiroDetenido = UnTiro 0 0 0

tunelConRampita :: Obstaculo
tunelConRampita = Obstaculo superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita unTiro = (precision unTiro) > 90 && vaAlRasDelSuelo unTiro

vaAlRasDelSuelo = (==0) . altura

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita unTiro = UnTiro {
    velocidad = (velocidad unTiro) * 2,
    precision = 100,
    altura = 0
}

laguna :: Int -> Obstaculo
laguna largoLaguna = Obstaculo superaLaguna (efectoLaguna largoLaguna)

superaLaguna :: Tiro -> Bool
superaLaguna unTiro = velocidad unTiro > 80 && between 1 5 (altura unTiro)

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largoLaguna unTiro = UnTiro {
    velocidad = velocidad unTiro,
    precision = precision unTiro,
    altura    = div (altura unTiro) largoLaguna
}

hoyo :: Obstaculo
hoyo = Obstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo unTiro = between 5 20 (velocidad unTiro) && vaAlRasDelSuelo unTiro && (precision unTiro) > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

-- PUNTO 4
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (leSirveParaSuperar unJugador unObstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar unJugador unObstaculo unPalo = puedeSuperarlo unObstaculo . golpe unJugador $ unPalo

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera unTiro [] = 0
cuantosObstaculosConsecutivosSupera unTiro (x:xs) 
 | puedeSuperarlo x unTiro = 1 + cuantosObstaculosConsecutivosSupera (efecto x unTiro) xs
 | otherwise               = 0

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador obstaculos = maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos . golpe unJugador) palos

-- PUNTO 5

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = map (padre . fst) . filter (not . gano puntosDeTorneo) $ puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = (all ((< snd puntosDeUnJugador).snd) . filter (/= puntosDeUnJugador)) puntosDeTorneo
