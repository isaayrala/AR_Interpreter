module CsvToHaskell where 
import Data.List
import Lang
import Text.Read (readMaybe)
import qualified Data.Map as Map

-- 1. toValue: Ahora procesa una celda (String) y decide el tipo
toValue :: String -> Value
toValue s = case readMaybe s :: Maybe Double of
              Just n  -> VDouble n
              Nothing -> VString s

-- 2. splitByComma: Tu lógica de split (está perfecta)
splitByComma :: String -> [String]
splitByComma [] = []
splitByComma s =
  let (campo, resto) = break (== ',') s
  in campo : case resto of
               []     -> []
               (_:xs) -> splitByComma xs

-- 3. zipear: Crea el Map para una sola fila
zipear :: [String] -> [Value] -> Map.Map String Value
zipear columnas valores = Map.fromList (zip columnas valores)

-- repasar monadas!!!
convert :: String -> String-> IO Table
convert s n= do
  content <- readFile s
  let filas = lines content
  let tablaRaw = map splitByComma filas
  
  if null tablaRaw 
    then error "El archivo está vacío"
    else do
      let columnas = head tablaRaw
      let datosStr = tail tablaRaw -- Filas sin el encabezado
      
      -- PASO CLAVE: Convertimos cada celda de cada fila a Value
      let filasValues = map (map toValue) datosStr
      
      let table = Table
                    { name    = n,
                      columns = columnas,
                      -- Zipeamos cada fila de Values con los nombres de columnas
                      rows    = map (zipear columnas) filasValues
                    }
      return table