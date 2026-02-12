module Prueba where
import Lang
import Parser
import Eval 
import qualified Data.Map as Map
import CsvToHaskell
import MonadM
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
-- Función auxiliar para pedir tabla
pedirTabla :: String -> EvalM Table
pedirTabla nombre = EvalM $ \env -> Map.lookup nombre env

init :: IO ()
init = do
    putStrLn "Bienvenido al Intérprete SQL"
    -- Iniciamos el bucle con un contexto vacío
    -- esto habria que cambiarlo, el contexto tiene que arrancar con las tablas S,P,J 
    -- y habria que llamar a convert (archivo)
    tablaS <- (convert "tables/S.csv" "S")
    tablaP <- (convert "tables/P.csv" "P")
    tablaJ <- (convert "tables/J.csv" "J")
    let m0 = (Map.insert "S" tablaS Map.empty) 
    let m1 = (Map.insert "P" tablaP m0) 
    let m2 = (Map.insert "J" tablaJ m1) 
    buclePrincipal m2


buclePrincipal :: Env -> IO ()
-- runInputT defaultSettings envuelve el bucle para habilitar las funciones pro
buclePrincipal env = runInputT defaultSettings (loop env)
  where
    loop :: Env -> InputT IO ()
    loop actualEnv = do
        -- getInputLine reemplaza al putStr + getLine
        minput <- getInputLine "SQL> "
        case minput of
            Nothing -> return () -- El usuario apretó Ctrl+D para salir
            Just "SALIR" -> return ()
            Just input -> do
                -- Lógica del parser
                let rel = case runRel input of
                                Right r -> r 
                                Left _  -> T "Error" -- O manejar el error prolijo
                
                -- Evaluación
                case runEval (eval rel) actualEnv of
                    Just tablaRes -> do
                        liftIO $ print tablaRes -- liftIO porque estamos en InputT
                        loop actualEnv
                    Nothing -> do
                        liftIO $ putStrLn "Error en la consulta"
                        loop actualEnv