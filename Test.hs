module Test where
import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import Lang
import Data.Map as Map
import Prettyprinter.Render.Text (renderStrict)
import Data.Text (unpack)
-- 1. Una función para convertir TU Value a Doc
prettyValue :: Maybe Value -> Doc ann
prettyValue Nothing          = pretty "NULL"       -- Qué mostrar si no hay nada
prettyValue (Just (VInt num))   = pretty num          -- Si es número
prettyValue (Just (VString texto)) = pretty (show texto) -- Si es string
prettyValue (Just (VDouble d)) = pretty d
-- 1. Definimos una lista de Strings comunes
ppcolumns :: Table -> [Doc ann]
ppcolumns t = Prelude.map (\s -> fill 15 (pretty s)) (columns t)

pprows :: Table ->[Doc ann]
pprows t =   (Prelude.map (\r -> hsep (Prelude.map (\c -> fill 15 (prettyValue (Map.lookup c r)))(columns t))) (rows t))
--las filas de una tabla son una lista de MAPAS 
--estos mapas estan formados por String Value 


tableToDoc :: Table -> Doc ann
tableToDoc t = 
  let r1 = ppcolumns t    
      r2 = pprows t

  in vsep [ hsep r1   -- El encabezado horizontal
          , vsep r2   -- Las filas ya apiladas
          ]
    

renderTable :: Doc Ann -> String
renderTable = unpack
            . renderStrict
            . layoutSmart defaultLayoutOptions
