module Main3 where
import qualified Data.Map as Map
import Eval
import Lang
import MonadM

-- ===== TABLAS DE EJEMPLO =====
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

tablaExtras :: Table
tablaExtras =
  Table
    { name = "table2",
      columns = ["Vendedor", "Nombre"]
    , rows =
        [ Map.fromList [("Vendedor", VString "P1"), ("Nombre", VString "Camilo")]
        , Map.fromList [("Vendedor", VString "P2"), ("Nombre", VString "Juan")]
        ]
    }

-- ===== MAIN =====

main :: IO ()
main = do

  putStrLn "===== TABLA ORIGINAL ====="
  print tablaClientes

--   -- SELECT edad > 23
--   let cond1 = Gt (VCol "edad") (VInt 23)
--   let consultaSelect = Select cond1 (T tablaClientes)

--   putStrLn "\n===== SELECT edad > 23 ====="
--   print (eval consultaSelect)

--   -- PROYECT quitar columna edad
--   let consultaProy = Proyect ["edad"] (T tablaClientes)

--   putStrLn "\n===== PROYECT sin edad ====="
--   print (eval consultaProy)

--   -- UNION
--   let consultaUnion = Union (T tablaClientes) (T tablaExtras)

--   putStrLn "\n===== UNION ====="
--   print (eval consultaUnion)

--   -- DIF
--   let consultaDif = Dif (T tablaClientes) (T tablaExtras)

--   putStrLn "\n===== DIF ====="
--   print (eval consultaDif)

--   -- PRODUCTO CARTESIANO
--   let consultaProd = Prod (T tablaClientes) (T tablaExtras)

--   putStrLn "\n===== PRODUCTO ====="
--   print (eval consultaProd)

--   -- RENAME
--   let consultaRename = Rename "cliente" (T tablaClientes)

--   putStrLn "\n===== RENAME ====="
--   print (eval consultaRename)

--  -- INTERSECCION
--   let consultaIntersec = Intersec (T tablaClientes) (T tablaExtras)
--   putStrLn "\n===== INTERSECTION ====="
--   print (eval consultaIntersec)

--  -- DIVISION
--   let consultaDivide = Div (T tablaClientes) (T tablaExtras)
--   putStrLn "\n===== DIVISION ====="
--   print (eval consultaDivide)

--  -- PRODUCTO
--   let consultaProducto = NatProd (T tablaClientes) (T tablaExtras)
--   putStrLn "\n===== PRODUCTO ====="
--   print (eval consultaProducto)

   -- PRODUCTO
  let consultaAgrup = Agrup "carrera" (SUM "SUM")"Notas" (T "table1")
  putStrLn "\n===== GROUPBY ====="
  let mapita = Map.empty
  print (runEval (eval consultaAgrup) (Map.insert "table1" tablaClientes mapita) )