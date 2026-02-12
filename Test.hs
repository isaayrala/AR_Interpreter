module Test where
import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import Lang
import Data.Map as Map
tablaClientes :: Table
tablaClientes =
  Table
    { name = "table1",
      columns = ["Alumno", "carrera","Notas"]
    , rows =
        [ Map.fromList [("Alumno", VInt 1), ("carrera", VString "LCC"), ("Notas", VDouble 10.0)]
        , Map.fromList [("Alumno", VInt 2), ("carrera", VString "LCC"), ("Notas", VDouble 1.0)]
        , Map.fromList [("Alumno", VInt 3), ("carrera", VString "Ing"), ("Notas", VDouble 5.0)]
        , Map.fromList [("Alumno", VInt 4), ("carrera", VString "Ing"), ("Notas", VDouble 10.0)]
        ]
    }

-- 1. Una función para convertir TU Value a Doc
prettyValue :: Maybe Value -> Doc ann
prettyValue Nothing          = pretty "NULL"       -- Qué mostrar si no hay nada
prettyValue (Just (VInt num))   = pretty num          -- Si es número
prettyValue (Just (VString texto)) = pretty (show texto) -- Si es string
prettyValue (Just (VDouble d)) = pretty d
-- 1. Definimos una lista de Strings comunes
ppcolumns :: Table -> [Doc ann]
ppcolumns t = Prelude.map (\s -> pretty s) (columns t)

pprows :: Table ->[Doc ann]
pprows t =   (Prelude.map (\r -> hsep (Prelude.map (\c -> prettyValue (Map.lookup c r))(columns t))) (rows t))
--las filas de una tabla son una lista de MAPAS 
--estos mapas estan formados por String Value 


main :: IO ()
main = do
    -- EJEMPLO HSEP: Como una oración o una fila
    let r1 = ppcolumns tablaClientes

    putStrLn "Resultado:"
 -- Los separa con un espacio: "Manzana Banana Cereza"
    
    putStrLn "\n"
    let r2 = pprows tablaClientes

    let tablaFinal = vsep [ hsep r1   -- El encabezado horizontal
                          , vsep r2   -- Las filas ya apiladas
                          ]
    
    putDoc tablaFinal -- Los separa con un salto de línea
    


