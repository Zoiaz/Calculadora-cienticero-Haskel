module Calculadora.Parsing (evaluarExpresion) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

import qualified Calculadora.Basicas as Bas
import qualified Calculadora.Trigonometria as Tri
import qualified Calculadora.Estadistica as Est

import Control.Monad (void)
import Data.Functor.Identity (Identity)

-- === Lexer ===
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "-", "*", "/", "^", ","]
    names = ["sin", "cos", "tan", "sinh", "cosh", "tanh", "log", "log10", "sqrt", "fact",
             "mean", "median", "stddev", "perm", "comb", "binom"]
    style = emptyDef { Tok.reservedOpNames = ops
                     , Tok.reservedNames = names }

integer    = Tok.integer lexer
float      = Tok.float lexer
parens     = Tok.parens lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
comma      = Tok.comma lexer
whiteSpace = Tok.whiteSpace lexer

-- === AST ===
data Expr
    = Num Double
    | Neg Expr
    | BinOp String Expr Expr
    | Func String Expr
    | Func2 String Expr Expr
    | FuncList String [Expr]
    deriving Show

-- === Parser ===
expr :: Parser Expr
expr = Ex.buildExpressionParser table term

table :: Ex.OperatorTable String () Identity Expr
table = [ [ prefix "-" Neg
          , prefix "+" id ]
        , [ binary "^" (BinOp "^") Ex.AssocRight ]
        , [ binary "*" (BinOp "*") Ex.AssocLeft
          , binary "/" (BinOp "/") Ex.AssocLeft ]
        , [ binary "+" (BinOp "+") Ex.AssocLeft
          , binary "-" (BinOp "-") Ex.AssocLeft ]
        ]

binary name fun = Ex.Infix (reservedOp name >> return fun)
prefix name fun = Ex.Prefix (reservedOp name >> return fun)

term :: Parser Expr
term =  try function2
    <|> try listFunc
    <|> try function
    <|> parens expr
    <|> fmap (Num . fromInteger) integer
    <|> fmap Num float

-- Funciones estadísticas que aceptan múltiples argumentos
listFunc :: Parser Expr
listFunc = do
    name <- choice (map parseFuncName ["mean", "median", "stddev"])
    xs <- parens $ expr `sepBy1` reservedOp ","
    return $ FuncList name xs

-- Funciones con un argumento
function :: Parser Expr
function = do
    name <- choice (map parseFuncName
        ["sin", "cos", "tan", "sinh", "cosh", "tanh", "log", "log10", "sqrt", "fact"])
    x <- parens expr
    return $ Func name x

-- Funciones con dos argumentos
function2 :: Parser Expr
function2 = do
    name <- choice (map parseFuncName ["perm", "comb", "binom"])
    args <- parens $ do
        a <- expr
        _ <- reservedOp ","
        b <- expr
        return (a, b)
    return $ uncurry (Func2 name) args

parseFuncName :: String -> Parser String
parseFuncName fname = reserved fname >> return fname

-- === Evaluación ===
eval :: Expr -> Either String Double
eval (Num x) = Right x
eval (Neg e) = fmap negate (eval e)
eval (BinOp op e1 e2) = do
    x <- eval e1
    y <- eval e2
    case op of
        "+" -> Right (x + y)
        "-" -> Right (x - y)
        "*" -> Right (x * y)
        "/" -> if y == 0 then Left "División por cero" else Right (x / y)
        "^" -> Right (x ** y)
        _   -> Left $ "Operador no soportado: " ++ op
eval (Func f x) = do
    v <- eval x
    case f of
        "sin"   -> Right (Tri.seno v)
        "cos"   -> Right (Tri.coseno v)
        "tan"   -> Right (Tri.tangente v)
        "sinh"  -> Right (Tri.sinh' v)
        "cosh"  -> Right (Tri.cosh' v)
        "tanh"  -> Right (Tri.tanh' v)
        "log"   -> if v <= 0 then Left "Log inválido" else Right (log v)
        "log10" -> if v <= 0 then Left "Log10 inválido" else Right (logBase 10 v)
        "sqrt"  -> Bas.raizCuadrada v
        "fact"  -> if v < 0 || fromIntegral (floor v) /= v
                   then Left "Factorial requiere entero positivo"
                   else Right . fromIntegral . Bas.factorial . floor $ v
        _       -> Left $ "Función no reconocida: " ++ f
eval (Func2 f x1 x2) = do
    v1 <- eval x1
    v2 <- eval x2
    case f of
        "perm"  -> toDouble $ Est.permutaciones (round v1) (round v2)
        "comb"  -> toDouble $ Est.combinaciones (round v1) (round v2)
        "binom" -> toDouble $ Est.coeficienteBinomial (round v1) (round v2)
        _       -> Left $ "Función binaria no reconocida: " ++ f
eval (FuncList f xs) = do
    vs <- mapM eval xs
    case f of
        "mean"   -> Est.promedio vs
        "median" -> Est.mediana vs
        "stddev" -> Est.desviacionEstandar vs
        _        -> Left $ "Función estadística múltiple no reconocida: " ++ f

toDouble :: Either String Integer -> Either String Double
toDouble = fmap fromIntegral

-- === Interfaz pública ===
evaluarExpresion :: String -> Either String Double
evaluarExpresion input =
    case parse (whiteSpace >> expr <* eof) "<input>" input of
        Left err -> Left (show err)
        Right e  -> eval e
