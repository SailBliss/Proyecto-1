import System.IO --abrir leer y escribir archivos
import System.Directory (doesFileExist) --doesFileExist verifica si un archivo existe en el sistema
import Data.Time.Clock
import Control.Exception (bracket) --usamos bracket para evitar fugas de recursos ya que maneja la apertura y el cierre de estos (en este caso lo añadimos para asegurarnos de que university.txt se cierre correctamente)
import Text.Read (readMaybe)

--definición de la estructura del tipo "estudiante" para representar los datos del estudiante
data Estudiante = Estudiante {
    idEstudiante :: String, -- el id del estudiante
    horaEntrada :: UTCTime, --hora de entrada del estudiante
    horaSalida :: Maybe UTCTime --hora de salida del estudiante
} deriving (Show, Read)

--indicamos la ruta del archivo donde se guardará la información del estudiante
archivo :: FilePath
archivo = "University.txt"

-- funcion para guardar la información dentro del archivo .txt
guardarEstudiantes :: [Estudiante] -> IO () --toma la lista de estudiante y devuelve un valor que no es importante
guardarEstudiantes estudiantes = bracket (openFile archivo WriteMode) hClose (\handle -> do
    hPrint handle estudiantes)

-- funcion para cargar y leer informacion dentro del archivo .txt
leerEstudiantes :: IO [Estudiante] --devuelve lista estudiante
leerEstudiantes = do
    existeArchivo <- doesFileExist archivo --verifica si el archivo existe
    if existeArchivo 
        then bracket (openFile archivo ReadMode) hClose (\handle -> do --usamos bracket para asegurarnos de que el archivo se cierre despues de usarlo
            contenido <- hGetContents handle
            length contenido `seq` return (case readMaybe contenido of
                Just estudiantes -> estudiantes
                Nothing -> []))  -- Si `read` falla, devolvemos una lista vacía en vez de que el programa crashee (mejora sugerida por IA)
        else return []

--funcion que registra el horario de entrada del estudiante
entrada :: [Estudiante] -> IO [Estudiante]
entrada estudiantes = do 
    putStr "Ingrese el ID del estudiante: "
    hFlush stdout --imprime el mensaje antes de esperar la entrada
    idIngresado <- getLine --obtiene el id ingresado por el usuario
    horaActual <- getCurrentTime --obtiene la hora actual 
    let nuevoEstudiante = Estudiante idIngresado horaActual Nothing
    let listaActualizada = nuevoEstudiante : estudiantes
    guardarEstudiantes listaActualizada
    putStrLn "Estudiante registrado!"
    return listaActualizada

--funcion para buscar estudiante por ID
buscarEstudiante :: [Estudiante] -> IO()
buscarEstudiante estudiantes = do
    putStr "Ingrese el ID del estudiante: "
    hFlush stdout
    idIngresado <- getLine
    case filter (\e -> idEstudiante e == idIngresado && horaSalida e == Nothing) estudiantes of --filter recorre toda la lista y devuelve solo los que cumplan con esta condicion
        [] -> putStrLn "No se pudo encontrar al estudiante/ya ha salido de la universidad" -- [] = no hay e en la lista
        [e] -> putStrLn $ "Estudiante encontrado: " ++ show e -- se encontro e en la lista
        _ -> putStrLn "Error: Se encontraron duplicados del estudiante" --cualquier otro caso

--funcion para calcular el tiempo que ha pasado el estudiante en la universidad
calcularTiempo :: [Estudiante] -> IO [Estudiante]
calcularTiempo estudiantes = do
    putStr "Ingrese el ID del estudiante: "
    hFlush stdout
    idIngresado <- getLine
    let registros = filter (\e -> idEstudiante e == idIngresado) estudiantes  --filtramos todas las entradas del estudiante
    case registros of
        [] -> putStrLn "No se ha encontrado al estudiante."
        _  -> do
            let tiempos = [diffUTCTime hs (horaEntrada e) | e <- registros, Just hs <- [horaSalida e]]  -- calculamos tiempos de salida
            if null tiempos
                then putStrLn "El estudiante aún no ha salido de la universidad."
                else do
                    let tiempoTotal = sum tiempos
                    putStrLn $ "Tiempo total dentro de la universidad: " ++ show tiempoTotal ++ " segundos"
    return estudiantes  -- la lista no cambia, solo la devolvemos igual

--funcion que nos da la lista de estudiantes 
listaEstudiantes :: [Estudiante] -> IO ()
listaEstudiantes estudiantes = do
    putStrLn "Estudiantes registrados: "
    if null estudiantes
        then putStrLn "No hay estudiantes registrados."
        else mapM_ print estudiantes --imrpime la lista de estudiantes

--funcion que registra el horario de salida del estudiante
salida :: [Estudiante] -> IO [Estudiante]
salida estudiantes = do
    putStr "Ingrese el ID del estudiante: "
    hFlush stdout
    idIngresado <- getLine --get line espera a que el usuario presione enter
    horaActual <- getCurrentTime
    let listaActualizada = map (\e -> if idEstudiante e == idIngresado && horaSalida e == Nothing --recorre cada estudiante (e) y modifica solo el que coincida con el id ingresado
        then  e {horaSalida = Just horaActual} -- si el estudiante no ha salido de la universidad y el id coincide modificamos la lista
        else e) estudiantes -- si no coincide o ya salio lo dejamos igual 
    guardarEstudiantes listaActualizada
    putStrLn "El estudiante ha salido de la universidad."
    return listaActualizada

--menu de opciones para el usuario 
menu :: [Estudiante] -> IO ()
menu estudiantes = do
    putStrLn "Menú de Estudiantes"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Buscar estudiante por ID"
    putStrLn "3. Calcular tiempo dentro de la universidad"
    putStrLn "4. Ver lista de estudiantes"
    putStrLn "5. Registrar salida de estudiante"
    putStrLn "6. Salir del programa "
    putStr "Selecciona una opción (1-6): "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> entrada estudiantes >>= menu
        "2" -> buscarEstudiante estudiantes >> menu estudiantes
        "3" -> calcularTiempo estudiantes >> menu estudiantes
        "4" -> listaEstudiantes estudiantes >> menu estudiantes
        "5" -> salida estudiantes >>= menu
        "6" -> putStrLn "Saliendo del programa..."
        _ -> putStrLn "Debes ingresar una opción válida!" >> menu estudiantes

-- funcion main
main :: IO ()
main = do
    estudiantes <- leerEstudiantes --cargamos los estudiantes guardados en el archivo
    menu estudiantes -- mostramos el menu