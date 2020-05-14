import Text.Show.Functions()

main :: IO ()
main = return ()

--Constructor
data Participante = Participante {
  nombre :: String,
  cantidadDeDinero :: Int,
  tacticaDeJuego :: String,
  propiedadesCompradas :: [Propiedad],
  accionesDeJuego :: [Accion]
} deriving (Show)

type Propiedad = (String, Int) --Nombre y precio
type Accion = (Participante -> Participante)


--Crear participantes
carolina :: Participante
carolina = Participante "Carolina" 500 "Accionista" [] [pasarPorElBanco,pagarAAccionistas]

manuel :: Participante
manuel = Participante "Manuel" 500 "Oferente singular" [] [pasarPorElBanco,enojarse]


--Cambiar datos
cambiarNombre :: (String -> String) -> Accion
cambiarNombre unaFuncion unParticipante = unParticipante { nombre = (unaFuncion . nombre) unParticipante }

cambiarCantidadDeDinero :: (Int -> Int) -> Accion
cambiarCantidadDeDinero unaFuncion unParticipante = unParticipante { cantidadDeDinero = (unaFuncion . cantidadDeDinero) unParticipante }

cambiarTacticaDeJuego :: (String -> String) -> Accion
cambiarTacticaDeJuego unaFuncion unParticipante = unParticipante { tacticaDeJuego = (unaFuncion . tacticaDeJuego) unParticipante }

cambiarPropiedad :: ([Propiedad] -> [Propiedad]) -> Accion
cambiarPropiedad unaFuncion unParticipante = unParticipante { propiedadesCompradas = (unaFuncion . propiedadesCompradas) unParticipante } 

cambiarAcciones :: ([Accion] -> [Accion]) -> Accion
cambiarAcciones unaFuncion unParticipante = unParticipante { accionesDeJuego = (unaFuncion . accionesDeJuego) unParticipante }


--Acciones
--1
pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = (cambiarCantidadDeDinero (+40) . cambiarTacticaDeJuego (const "Comprador compulsivo")) unParticipante

--2
enojarse :: Accion
enojarse unParticipante = (cambiarCantidadDeDinero (+50) . cambiarAcciones (++ [gritar])) unParticipante

--3
gritar :: Accion
gritar unParticipante = cambiarNombre ("AHHH " ++) unParticipante 

--4
tieneTactica :: String -> Participante -> Bool
tieneTactica unaTactica unParticipante = ((== unaTactica) . tacticaDeJuego) unParticipante

esGanador :: Participante -> Bool
esGanador unParticipante = (tieneTactica "Accionista" unParticipante) || (tieneTactica "Oferente singular" unParticipante)

ganarPropiedad :: Propiedad -> Accion
ganarPropiedad unaPropiedad unParticipante = (cambiarCantidadDeDinero (subtract (snd unaPropiedad)) . cambiarPropiedad (++ [unaPropiedad]) ) unParticipante

subastar :: Propiedad -> Accion
subastar unaPropiedad unParticipante
  | esGanador unParticipante = ganarPropiedad unaPropiedad unParticipante
  | otherwise                = unParticipante

--5
gananciaPorPropiedad :: Propiedad -> Int
gananciaPorPropiedad unaPropiedad
  | (snd unaPropiedad) < 150  = 10
  | otherwise                 = 20

cantidadACobrar :: [Propiedad] -> Int
cantidadACobrar listaPropiedades = (sum . map gananciaPorPropiedad) listaPropiedades

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = cambiarCantidadDeDinero (+ cantidadACobrar (propiedadesCompradas unParticipante)) unParticipante
  
--6
pagarAAccionistas :: Accion
pagarAAccionistas unParticipante
  | tieneTactica "Accionista" unParticipante = cambiarCantidadDeDinero (+200) unParticipante
  | otherwise                                = cambiarCantidadDeDinero (subtract 100) unParticipante

--7
conseguirDinero :: Propiedad -> Accion
conseguirDinero unaPropiedad unParticipante
  | cantidadDeDinero unParticipante == snd unaPropiedad = ganarPropiedad unaPropiedad unParticipante
  | otherwise                                           = ((conseguirDinero unaPropiedad) . cambiarCantidadDeDinero (+10)) unParticipante

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unParticipante = ((conseguirDinero unaPropiedad) . gritar) unParticipante

--8
ultimaRonda :: Accion
ultimaRonda unParticipante = (foldl1 (.) (reverse (accionesDeJuego unParticipante))) unParticipante

dineroAlFinalDelJuego :: Participante -> Int
dineroAlFinalDelJuego unParticipante = (cantidadDeDinero . ultimaRonda) unParticipante

anunciarGanador :: Participante -> String
anunciarGanador unParticipante = "The winner is... " ++ (nombre unParticipante)

finalDelJuego :: Participante -> Participante -> String
finalDelJuego participanteUno participanteDos
  | dineroAlFinalDelJuego participanteUno >= dineroAlFinalDelJuego participanteDos = anunciarGanador participanteUno
  | otherwise                                                                      = anunciarGanador participanteDos
