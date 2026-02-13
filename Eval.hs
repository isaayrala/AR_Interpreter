module Eval where
import Lang
import qualified Data.Map as Map
import Data.List ((\\), nub,intersect)
import Debug.Trace
import MonadM
import TableOps

failEval :: EvalM a
failEval = EvalM (\_ -> Nothing)


eval :: Rel -> EvalM Table
eval (T s) = do e <- getEnv
                case (Map.lookup s e) of
                  Nothing -> failEval
                  (Just t) -> return t

eval (Select c r) = do
    t <- eval r
    if passCond c t
        then return (select c t)
        else failEval

                       
-- deja las filas aquellas las cuales van a cumplir un predicado
-- capaz que hay que hacer una funcion auxiliar para ver q hizo filter?
eval (Proyect l r) =    if (null l) then do failEval
                        else do t <- eval r
                                if (noMatch l t) then do failEval else do return (proyect l t)
                      
eval (Union rel1 rel2) = do t <- eval rel1 
                            t' <- eval rel2
                          --if checktysss t t'
                            return  (unitables t t')
                          --else return (Just [])


eval (Dif rel1 rel2) = do t <- eval rel1 
                          t' <- eval rel2
                          return  (diftables t t')


eval (Prod rel1 rel2) = do t <- eval rel1 
                           t' <- eval rel2
                           return  (multables t t' False)

eval (Rename name rel) = do 
    t <- eval rel
    e <- getEnv
    if (validName name e) then return  (rename name t)
    else failEval

eval (Intersec rel1 rel2) = eval (Dif rel1 (Dif rel1 rel2))


eval (Div rel1 rel2) = do
  t1 <- eval rel1
  t2 <- eval rel2
  return (division t1 t2)

eval (NatProd rel1 rel2) = do
  t1 <- eval rel1
  t2 <- eval rel2

  let cols1 = columns t1
      cols2 = columns t2
      i     = intersect cols1 cols2

  case (rows t1, rows t2) of
    ([], _) -> return (multables t1 t2 True)
    (_, []) -> return (multables t1 t2 True)

    (r1:_, r2:_) ->
      if null i
         then return (multables t1 t2 True)
         else if checkDomains i r1 r2
                 then return (prodnat t1 t2)
                 else failEval
  
eval (Agrup s1 o s2 rel1) = do 
	t1 <- eval rel1
  if s1 `notElem` columns t1 || s2 `notElem` columns t1
   then failEval	
  else if (isnum s2 o t1) then return (evalGroupBy [s1] o s2 t1)
       else  failEval
-- si s1 y s2 no son elem de columns de t1 entonces no se hace nada 
-- por otro lado si s2 no es numero entonces no se le puede aplicar ninguna operacion q sea numerica como MAX, MIN, AVG, SUM 


--esto quizas se puede chequear de otra forma

------------------------
-- FUNCIONES AUXILIARES 
------------------------
isnum :: String -> Op -> Table -> Bool
isnum s op t1 =
  case rows t1 of
    [] -> True  -- tabla vacía: no hay nada que invalide el tipo
    (r:_) ->
      case Map.lookup s r of
        Just (VInt _)    -> True
        Just (VDouble _) -> True
        Just (VString _) ->
          case op of
            COUNT _ -> True
            _       -> False
        _ -> False

noMatch :: [String] -> Table -> Bool
noMatch l t = foldr (&&) True (map (\s -> s `elem` (columns t)) l)

validName :: String -> Map.Map String Table -> Bool
validName s m = case (Map.lookup s m) of 
                  (Just x) -> False
                  Nothing -> True 

passCond :: Cond -> Table -> Bool
passCond (Eq (VCol s) v) t = foldr (||) False (map (\c -> s == c ) (columns t))
-- tengo q ver si la vcol de la izq matchea con algun valor de las columnas de la tabla

checkDomains :: [String] -> Row-> Row -> Bool
checkDomains l m1  m2 = foldr (&&) True( map (\i -> checkVals (Map.lookup i m1) (Map.lookup i m2) ) l)

checkVals :: Maybe Value ->Maybe Value -> Bool
checkVals (Just (VInt _)) (Just (VInt _))  = True
checkVals (Just (VDouble _)) (Just (VDouble _))  = True
checkVals (Just (VString _)) (Just (VString _))  = True
checkVals _  _= False
  