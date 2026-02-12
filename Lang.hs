module Lang where
import qualified Data.Map as M

type Atributo = String
type Archivo = String


data Cond
  = Eq Value Value
  | InEq Value Value
  | Geq Value Value
  | Leq Value Value 
  | Gt Value Value 
  | Lt Value Value 
  | And Cond Cond
  | Or Cond Cond
  deriving Show

data Value
  = VDouble Double
  | VString String
  | VCol String
  | VInt Int
  deriving (Show, Ord, Eq)

data Op 
  = AVG String
  | MAX String
  | MIN String
  | SUM String
  | COUNT String
  deriving Show

data Rel 
  = T String 
  | Select Cond Rel 
  | Proyect [String] Rel 
  | Union Rel Rel 
  | Dif Rel Rel 
  | Prod Rel Rel 
  | Rename String Rel  
  | Intersec Rel Rel 
  | NatProd Rel Rel 
  | Agrup String Op String Rel --agrupar por carrera, promedio, notas
  | Div Rel Rel 
  deriving Show

type Row = M.Map String Value
data Table = Table
  { name :: String,
    columns :: [String]
  , rows    :: [Row]
  } deriving Show

emptyTable = Table
  { name = "",
    columns = [],
    rows = []
}

type Env = M.Map String Table
-- tengo que describir una expresion para las op:
-- select
-- proyecc
-- union
-- diferencia
-- product cart
-- renombramiento

-- operaciones que se definen en base a otras operaciones
-- intersec
-- union nat o producto nat
-- division
-- agrupamiento

-- que devuelven las consultas?
-- una lista de columnas o una columna sola
-- una lista de filas enteras o una fila sola

-- como se representa una tabla?
-- la forma mas facil me parece q es con una string
