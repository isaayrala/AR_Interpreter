checker :: Rel -> EvalM Rel 
checker (T s) = e <- getEnv
checker (Select c r) = do c'<- checkerCond c 
                          r'<- checker r
                          return (Select c' r')
-- condicion con cosas que nqv 
checker (Proyect l r) = if not null l && 
  -- lista vacia
  -- atributos q no esten en mi tabla! eso principalmente
checker (Agrup s o n r) = do 
                             s' <- checkAttr s t
                             n' <- checkAttr s t
-- aca lo mismo, los atributos deben estar en la tabla
                             return (Agrup s' o n' r')
checker (Rename n r) = -- no se puede renombrar con algo q este en el contexto !
checker (Union r r') = checker r 
checker (Dif r r') = checker 
checker (Prod r r') = 
checker (Union r r') = 
checker (Intersec r r') = 
checker (NatProd r r') = 
checker (Div r r') = 

checkerCond :: Cond -> Table -> Maybe Cond
checker c@(Eq v1 v2) t = do if (not (v1 `elem` (columns t))) then Nothing
                            else return c
checker c@(InEq v1 v2) t = do if (not (v1 `elem` (columns t))) then Nothing
                              else return c
checker c@(Gt v1 v2) t = do if (not (v1 `elem` (columns t))) then Nothing
                            else return c    
checker c@(Lt v1 v2) t = do if (not (v1 `elem` (columns t))) then Nothing
                            else return c      
checker c@(Geq v1 v2) t = do if (not (v1 `elem` (columns t))) then Nothing
                            else return c   
checker c@(Leq v1 v2) t = do if (not (v1 `elem` (columns t))) then Nothing
                            else return c          
checker c@(And v1 v2) t = do c <- checker c1
                            c'<- checker c2
                            return (And c c')
checker c@(Or v1 v2) t = do c <- checker c1
                            c'<- checker c2
                            return (And c c')


extraerColumn:: Value -> String
extraerColumn (VCol s) = S
extraerColumn _ = error               