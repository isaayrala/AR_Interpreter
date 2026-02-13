module TableOps where
import Lang 
import qualified Data.Map as Map
import Data.List ((\\), nub,intersect)

prodnat :: Table -> Table -> Table
prodnat t1 t2 = 
	let
		cols1 = (columns t1)
		cols2 = (columns t2)
      -- if t1 o t2 vacio entonces no hago mas nada corto retornando vacia
      -- eso se manejaria en multables
		u =  nub(cols1++cols2)
		i = intersect cols1 cols2
        a = (armarpred i (name t1) (name t2))
        p = multables t1 t2 True
        s = select a p
    in proyect u s
           

-- podria definirme una aux 
-- que tome los de la lista de interseccion
-- 
-- eval (Div rel1 rel2) = let rest = (columns rel1)\\(columns rel2)
--       in eval (Dif p1 (Proyect rest (Dif (Prod (Proyect rest rel1 ) rel2) rel1)))
      
division :: Table -> Table -> Table
division t1 t2 =
  let
    cols1 = columns t1
    cols2 = columns t2
    rest  = cols1 \\ cols2
		
    p1    = proyect rest t1
    p3    = multables p1 t2 False
    d1    = diftables p3 t1
    p2    = proyect rest d1
    -- resultado = trace ("p1 = " ++ show p1++ "\np2 = "++ show p2++ "\np3 = "++show p3++ "\nd1 = "++ show d1++ "\n resultado= "++ show resultado) (diftables p1 p2)
		-- rever el proyect
	in diftables p1 p2


armarpred :: [String] -> String -> String -> Cond
-- va a tomar una lista de atributos y los nombres de las tablas
-- debe armar todos and
-- r.atributoi = s.atributoi
armarpred [x] n1 n2 =  (Eq (VCol (n1++"." ++x)) (VCol (n2++"."++x)))
armarpred (x:xs) n1 n2 = (And (Eq (VCol (n1++"." ++x)) (VCol (n2++"."++x))) (armarpred xs n1 n2))
--aca hay que tener cuidado, capaz que hay que hacer renombre por el tema de como quedan los attr.


select :: Cond -> Table -> Table
select c t = 
      Table {
            name = (name t),
            columns = columns t,
            rows = filter (\f -> evalcond c f) (rows t)
      }
-- ===== RESOLVER VALORES =====

resolve :: Map.Map String Value -> Value -> Maybe Value
resolve env (VCol c) = Map.lookup c env
resolve _   v        = Just v


-- ===== FUNCIÓN AUXILIAR PARA COMPARAR =====

compareVals
  :: (Value -> Value -> Bool)
  -> Value
  -> Value
  -> Map.Map String Value
  -> Bool
compareVals op v1 v2 env =
  case (resolve env v1, resolve env v2) of
    (Just r1, Just r2) -> op r1 r2
    _                  -> False

-- ===== EVALUADOR PRINCIPAL =====

evalcond :: Cond -> Map.Map String Value -> Bool
evalcond (Eq v1 v2)   env = compareVals (==) v1 v2 env
evalcond (InEq v1 v2) env = compareVals (/=) v1 v2 env
evalcond (Geq v1 v2)  env = compareVals (>=) v1 v2 env
evalcond (Gt v1 v2)   env = compareVals (>)  v1 v2 env
evalcond (Lt v1 v2)   env = compareVals (<)  v1 v2 env
evalcond (Leq v1 v2)  env = compareVals (<=) v1 v2 env
evalcond (And c1 c2)  env = evalcond c1 env && evalcond c2 env
evalcond (Or c1 c2)   env = evalcond c1 env || evalcond c2 env

----------------------------
proyect :: [String] -> Table -> Table
--proyect [] t = error "no se puede proyectar vacio"
-- cuando hago proyect rest [S,P]
-- proyect [S]
proyect keys t = 
      Table {
            name = (name t),
            columns = intersect (columns t) keys,
            rows = nub (map (\m -> foldr Map.delete m ((columns t)\\keys)) (rows t))
      }

----------------------------

unitables :: Table -> Table -> Table
-- unitables t1 [] = t1
-- unitables [] t2 = t2
-- unitables [] [] = []
-- unitables t1@(l:ls) t2@(x:xs) = if samequeue l x then l 
--                                 else (l:x: (unitables ls xs))
-- where samequeue ((c,v):xs) ((c',v'): l) = samevalue v v' && samequeue l xs 
--       samevalue (_ v) (_ v') = v == v'
-- Esto funciona perfectamente con la estructura de Maps
unitables t1 t2 = 
      Table {
            name = (name t1)++ "U"++(name t2),
            columns = columns t1,
            rows = nub ((rows t1) ++ (rows t2))
      }

diftables t1 t2 = 
      Table {
            name = (name t1)++"D"++(name t2),
            columns = columns t1,
            rows = (rows t1) \\ (rows t2)
      }

multables t1 t2 b = 
	if (null i) then
    Table {
        name = (name t1)++"P"++(name t2),
        columns = columns t1 ++ columns t2,-- ++ (renamecol(columns t2) (name t2)),
        rows = concatMap (\r1 -> map (\r2 -> Map.union r1 r2) (rows t2)) (rows t1)
    }
	else 
		Table {
			name = (name t1)++"P"++(name t2),
			columns = (renamecol (columns t1) (name t1) i) ++ renamecol (columns t2) (name t2) i,
			rows = concatMap (\row1 -> map (\row2 -> Map.union row1 row2) r2) r1
    }
   
  where
    i  = intersect (columns t1) (columns t2)
    r1 = renombrarFilas t1 i
    r2 = renombrarFilas t2 i

renombrarFilas :: Table -> [String] -> [Map.Map String Value]
renombrarFilas t comunes =
  map renameRow (rows t)
  where
    n = name t
    renameRow r =
      Map.fromList
        [ (if k `elem` comunes then n ++ "." ++ k else k, v)
        | (k, v) <- Map.toList r
        ]

rename s t =
      Table {
            name = s,
            columns = map (\c -> new ++ c) (columns t),
            rows = map renameRow (rows t) -- ¡Obligatorio actualizar los datos!
      }
      where 
        new = s ++ "."
        renameRow r = Map.fromList [ (new ++ k, v) | (k, v) <- Map.toList r ]

renamecol :: [String] -> String -> [String] -> [String]
renamecol l n c = map (\s -> if s `elem` c then n++"."++s else s) l

-- L = ["carrera"], aggCol = "promedio"
evalGroupBy :: [String] -> Op -> String -> Table -> Table
evalGroupBy l o aggCol t = --proyect l t
  let
    -- PASO 1: Proyectar las claves únicas (p)
    claves = proyect l t
    
    -- PASO 2 y 3: Para cada fila de las claves, aplicar Select y calcular
    nuevasRows = map (\filaClave ->
        -- s = select (carrera == valor) tabla
        -- Aquí armarpred te sirve para crear el filtro dinámicamente
        let 
          predicado = armarFiltroExacto l filaClave 
          tablaGrupo = select predicado t
          
          -- Extraer los valores de la columna a promediar
          result = manejar o aggCol tablaGrupo
          
          -- Unir la clave con el resultado
        in Map.insert ((extraerString o) ++ aggCol) result filaClave
      ) (rows claves)
  in 
    Table {
      name = "GroupBy_" ++ (name t),
      columns = l ++ [(extraerString o)++ aggCol],
      rows = nuevasRows
    }

armarFiltroExacto :: [String] -> Map.Map String Value -> Cond
armarFiltroExacto [] _ = error "imposible"
armarFiltroExacto [x] m =
  let Just val = Map.lookup x m
  in Eq (VCol x) val

armarFiltroExacto (x:xs) m =
  let Just val = Map.lookup x m
  in And (Eq (VCol x) val) (armarFiltroExacto xs m)

extraerDouble :: Maybe Value -> Double
extraerDouble (Just (VDouble d)) = d
extraerDouble (Just (VInt n))    = fromIntegral n -- Por si las notas son enteros
extraerDouble _                  = 0.0            -- Si es Nothing o no es número

extraerInt :: Maybe Value -> Int
extraerInt (Just (VDouble d)) = round d
extraerInt (Just (VInt n))    = n  -- Por si las notas son enteros
extraerInt _                  = 0        -- Si es Nothing o no es número

extraerString :: Op -> String
extraerString (AVG s) = s
extraerString (SUM s) = s
extraerString (MAX s) = s
extraerString (MIN s) = s
extraerString (COUNT s) = s

manejar :: Op -> String -> Table -> Value
manejar op aggCol tablaGrupo = 
	let 
		valores = map (\r -> extraerDouble (Map.lookup aggCol r)) (rows tablaGrupo)
	in
		case op of 
			AVG c -> 
				if null valores then VDouble 0.0 
					else VDouble (sum valores / fromIntegral (length valores))

			SUM c-> 
				VDouble (sum valores)

			COUNT c-> 
				VInt (length (rows tablaGrupo))

			MAX c-> 
			 if null valores then VDouble 0 else VDouble (maximum valores)

			MIN c-> 
				if null valores then VDouble 0 else VDouble (minimum valores)


  -- l es una lista de columnas que es la intersec entre columnas de t1 y de t2 
  -- lo q queremos es chequear que para cada i, efectivamente estamos hablando del mismo tipo de valor
  -- la idea es 
  -- tomar un i de la l, y tomamos un m1 y chequeamos 

