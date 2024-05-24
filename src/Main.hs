import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception (catch, IOException)
import Text.Regex.Posix ((=~))
import Data.Maybe (mapMaybe)

-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el vehículo aún está en el parqueadero o ya salió
} deriving (Show, Read)  -- Agregamos Read aquí

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    Vehiculo placaVehiculo tiempo Nothing : parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    case salida vehiculo of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada vehiculo)
        Nothing           -> diffUTCTime tiempoActual (entrada vehiculo)

-- Función para guardar la información de los vehículos en un archivo de texto
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    writeFile "parqueadero.txt" (unlines (map show parqueadero))
    putStrLn "Parqueadero guardado en el archivo parqueadero.txt."

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    contenido <- catch (readFile "parqueadero.txt") manejarError
    let lineas = lines contenido
    return (if null lineas then [] else map read lineas)
    where
        manejarError :: IOException -> IO String
        manejarError _ = return ""

-- Función para mostrar la información de un vehículo como cadena de texto
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo vehiculo =
    placa vehiculo ++ "," ++ show (entrada vehiculo) ++ "," ++ show (salida vehiculo)

-- Función para validar el formato de la placa
validarPlaca :: String -> Bool
validarPlaca placa = placa =~ "^[A-Z]{3}-[0-9]{3}$"

-- Función para cargar la información de los vehículos desde un txt
leerParqueadero :: IO [Vehiculo]
leerParqueadero = do
    contenido <- readFile "parqueadero.txt"
    let lineas = lines contenido
    return (mapMaybe parsearVehiculo lineas)
    where
        parsearVehiculo :: String -> Maybe Vehiculo
        parsearVehiculo linea = case words linea of
            [placa, entrada, salida] -> Just $ Vehiculo placa (read entrada) (readMaybeSalida salida)
            _ -> Nothing

        readMaybeSalida :: String -> Maybe UTCTime
        readMaybeSalida "Nothing" = Nothing
        readMaybeSalida salidaStr = Just (read salidaStr)

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el parqueadero desde el archivo de texto
    parqueadero <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"

    -- Ciclo principal del programa
    cicloPrincipal parqueadero

-- Función para el ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Listar vehículos"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo (formato ABC-123):"
            placaVehiculo <- getLine
            if validarPlaca placaVehiculo
                then do
                    tiempoActual <- getCurrentTime
                    let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
                    putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
                    guardarParqueadero parqueaderoActualizado
                    cicloPrincipal parqueaderoActualizado
                else do
                    putStrLn "Formato de placa inválido. Intente nuevamente."
                    cicloPrincipal parqueadero

        "2" -> do
            if null parqueadero
                then putStrLn "No hay vehículos en el parqueadero."
                else do
                putStrLn "Ingrese la placa del vehículo a salir (formato ABC-123):"
                placaVehiculo <- getLine
                if validarPlaca placaVehiculo
                    then do
                        tiempoActual <- getCurrentTime
                        let parqueaderoActualizado = registrarSalida placaVehiculo tiempoActual parqueadero
                        putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " salido del parqueadero."
                        guardarParqueadero parqueaderoActualizado
                        cicloPrincipal parqueaderoActualizado
                    else do
                        putStrLn "Formato de placa inválido. Intente nuevamente."
                        cicloPrincipal parqueadero
            cicloPrincipal parqueadero

        "3" -> do
            if null parqueadero
                then putStrLn "No hay vehículos en el parqueadero."
                else do
                putStrLn "Ingrese la placa del vehículo a buscar (formato ABC-123):"
                placaVehiculo <- getLine
                if validarPlaca placaVehiculo
                    then do
                        tiempoActual <- getCurrentTime
                        case buscarVehiculo placaVehiculo parqueadero of
                            Just vehiculo -> do
                                let tiempoTotal = tiempoEnParqueadero vehiculo tiempoActual
                                putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                                putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                            Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
                        cicloPrincipal parqueadero
                    else do
                        putStrLn "Formato de placa inválido. Intente nuevamente."
                        cicloPrincipal parqueadero
            cicloPrincipal parqueadero

        "4" -> do
            if null parqueadero
                then putStrLn "No hay vehículos en el parqueadero."
                else do
                    putStrLn "Vehículos en el parqueadero:"
                    mapM_ (putStrLn . mostrarVehiculo) parqueadero
            cicloPrincipal parqueadero

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero
