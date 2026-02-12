{-# LANGUAGE FlexibleContexts #-}
module Parser where
import Text.Parsec.Expr
import Lang
import Control.Monad.Identity (Identity)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

type P = Parsec String ()

-- | Parser de términos
tm :: P Rel
tm = 
      parens tm'
  <|> tm' 
--chequear precedencias!

tm' :: P Rel 
tm' = uni <|> dif <|> prod <|> sel <|> proy <|> rename <|> tablename <|> agrup

langDef :: Tok.LanguageDef ()
langDef = emptyDef
  { Tok.reservedNames   = ["AGRUP","PROYECT", "SELECT", "UNION", "PROD", "NATPROD", "DIV","INTERSEC", "SUB","RENAME","AVG","MAX","MIN","SUM","COUNT","T"]
  , Tok.reservedOpNames = ["=","!=","<",">","<=",">=","&&","||"]
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

parens :: P a -> P a
parens = Tok.parens lexer

commaSep :: P a -> P [a]
commaSep = Tok.commaSep lexer

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

number :: P Double
number =
      try (Tok.float lexer)
  <|> (fromInteger <$> Tok.integer lexer)

-- Define el parser para strings usando tu lexer existente
stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer
-----------------------------------
--- Parser de seleccion ! CHEQUEAR !
-----------------------------------
-- necesito que cond lea una condicion 
-- la cual estara formada por
-- algo > algo
-- algo < algo
-- algo = algo
-- algo AND algo
-- algo OR algo 
-- algo <=, >=, <, > 
-- algo != algo 
operator :: P String
operator =
      try (reservedOp "<=" >> return "<=")
  <|> try (reservedOp ">=" >> return ">=")
  <|> try (reservedOp "!=" >> return "!=")
  <|> (reservedOp "="  >> return "=")
  <|> (reservedOp "<"  >> return "<")
  <|> (reservedOp ">"  >> return ">")
  <|> (reservedOp "&&" >> return "&&")
  <|> (reservedOp "||" >> return "||")


binary :: String -> (Cond -> Cond -> Cond) -> Assoc -> Operator String () Identity Cond
binary name f assoc = Infix (reservedOp name >> return f) assoc

table :: [[Operator String () Identity Cond]]
table =
  [ [ binary "&&" And AssocLeft
    , binary "||" Or  AssocLeft
    ]
  ]

condExpr :: P Cond
condExpr = buildExpressionParser table condTerm

condTerm =
      parens condExpr
  <|> cond

cond :: P Cond
cond = try $ do
  left  <- (VCol <$> identifier)
  o     <- operator
  
  right <- VString <$> stringLiteral
     <|> (VCol <$> identifier)
     <|> VDouble <$> number

  return (buildcond left o right)


buildcond :: Value -> String -> Value -> Cond
buildcond l op r = case op of
  "="  -> Eq l r
  "!=" -> InEq l r
  ">=" -> Geq l r
  "<=" -> Leq l r
  ">"  -> Gt l r
  "<"  -> Lt l r
  _    -> error "Operador inválido"

sel :: P Rel
sel = do
  reserved "SELECT"
  c <- parens condExpr
  t <- tm
  return (Select c t)
-----------------------------------
--- Parser de proyeccion
-----------------------------------

proy :: P Rel 
proy = do 
  reserved "PROYECT"
  a <- parens (commaSep identifier)

  t <- tm
  return (Proyect a t)

-----------------------------------
--- Parser de operaciones entre rel
-----------------------------------
uni :: P Rel 
uni = do 
  reserved "UNION"
  r <- tm
  r' <- tm
  return (Union r r')

dif :: P Rel 
dif = do 
  reserved "SUB"
  r <- tm
  r' <- tm
  return (Dif r r')

intersec :: P Rel
intersec = do 
  reserved "INTERSEC"
  r <- tm
  r' <- tm
  return (Intersec r r')

natprod :: P Rel 
natprod = do 
  reserved "NATPROD"
  r <- tm
  r' <- tm
  return (NatProd r r')

prod :: P Rel 
prod = do 
  reserved "PROD"
  r <- tm
  r' <- tm
  return (Prod r r')

div :: P Rel 
div = do 
  reserved "DIV"
  r <- tm
  r' <- tm
  return (Div r r')


agrup :: P Rel 
agrup = do 
  reserved "AGRUP"
  s <- identifier
  o <- operaciones
  i <- identifier
  r <- tm
  return (Agrup s o i r)


------------------------------------
-- Parseo de Renombre
------------------------------------
rename :: P Rel 
rename = do 
  reserved "RENAME"
  i <- identifier
  t <- tm
  return (Rename i t)

------------------------------------
-- Parseo de Tabla
------------------------------------
tablename:: P Rel
tablename = do 
  reserved "T"
  i <- identifier 
  return (T i)
------------------------------------
-- Función de parseo
------------------------------------
-- totParser :: Parser a -> Parser a
-- totParser p = do
--   whiteSpace langDef
--   t <- p
--   eof
--   return t

-- parseComm :: SourceName -> String -> Either ParseError Comm
-- parseComm = parse (totParser tm)

operaciones :: P Op
operaciones = 
      try (reserved "AVG" >> return (AVG "Avg"))
  <|> try (reserved "SUM" >> return (SUM "Sum"))
  <|> try (reserved "COUNT" >> return (COUNT "Count"))
  <|> (reserved "MAX"  >> return (MAX "Max"))
  <|> (reserved "MIN"  >> return (MIN "Min"))


runRel :: String -> Either ParseError Rel
runRel s =  runParser (whiteSpace *> tm <* eof) () "" s
            

