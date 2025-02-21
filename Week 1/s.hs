module Parser (ParserError, parseString) where

import Syntax
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)

-- Renamed to avoid clashing with Text.Parsec.ParseError
type ParserError = String

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops   = ["+", "-", "*", "/", "%", "==", "<", ">", "=", "|"]
    names = ["define", "execute", "not", "for", "in", "if", "None", "True", "False"]
    style = emptyDef {
              Tok.commentLine     = "#"
            , Tok.reservedOpNames = ops
            , Tok.reservedNames   = names
            }

integer :: Parser Integer
integer = Tok.integer lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- Parser for Value
parseValue :: Parser Value
parseValue =
      (reserved "None" >> return None)
  <|> (reserved "True" >> return (Boolean True))
  <|> (reserved "False" >> return (Boolean False))
  <|> (Number <$> integer)
  <|> (Text <$> stringLiteral)
  <|> parseListValue
  where
    parseListValue = do
      lst <- brackets (commaSep parseValue)
      return $ List lst

-- Parser for Expression
parseExpression :: Parser Expression
parseExpression = Ex.buildExpressionParser table term

table :: [[Ex.Operator String () Identity Expression]]
table = [ [Ex.Prefix (reservedOp "not" >> return Not)]
        , [Ex.Infix  (reservedOp "*" >> return (Operation Times)) Ex.AssocLeft,
           Ex.Infix  (reservedOp "/" >> return (Operation Div)) Ex.AssocLeft,
           Ex.Infix  (reservedOp "%" >> return (Operation Mod)) Ex.AssocLeft]
        , [Ex.Infix  (reservedOp "+" >> return (Operation Plus)) Ex.AssocLeft,
           Ex.Infix  (reservedOp "-" >> return (Operation Minus)) Ex.AssocLeft]
        , [Ex.Infix  (reservedOp "==" >> return (Operation Eq)) Ex.AssocNone,
           Ex.Infix  (reservedOp "<" >> return (Operation Less)) Ex.AssocNone,
           Ex.Infix  (reservedOp ">" >> return (Operation Greater)) Ex.AssocNone]
        ]

term :: Parser Expression
term =  parens parseExpression
    <|> try parseCall
    <|> try parseListComp
    <|> parseListExpr
    <|> (Constant <$> parseValue)
    <|> (Variable <$> identifier)

parseCall :: Parser Expression
parseCall = do
  fname <- identifier
  args  <- parens (commaSep parseExpression)
  return $ Call fname args

parseListExpr :: Parser Expression
parseListExpr = do
  exprs <- brackets (commaSep parseExpression)
  return $ ListExpression exprs

parseListComp :: Parser Expression
parseListComp = do
  char '['
  whiteSpace
  e <- parseExpression
  whiteSpace
  char '|'
  whiteSpace
  clauses <- commaSep parseClause
  whiteSpace
  char ']'
  return $ ListComprehension e clauses

parseClause :: Parser Clause
parseClause = try parseForClause <|> parseIfClause

parseForClause :: Parser Clause
parseForClause = do
  reserved "for"
  var  <- identifier
  reserved "in"
  expr <- parseExpression
  return $ For var expr

parseIfClause :: Parser Clause
parseIfClause = do
  reserved "if"
  expr <- parseExpression
  return $ If expr

-- Parser for Statement
parseStatement :: Parser Statement
parseStatement = try parseDefine <|> parseExecute

parseDefine :: Parser Statement
parseDefine = do
  reserved "define"
  var <- identifier
  reservedOp "="
  expr <- parseExpression
  return $ Define var expr

parseExecute :: Parser Statement
parseExecute = do
  reserved "execute"
  expr <- parseExpression
  return $ Execute expr

parseProgram :: Parser Program
parseProgram = whiteSpace >> many (parseStatement <* optional semi) <* eof
  where semi = Tok.semi lexer

parseString :: String -> Either ParserError Program
parseString input =
  case parse parseProgram "" input of
    Left err   -> Left (show err)
    Right prog -> Right prog


module Interpreter where

import Syntax
import Control.Monad
import Prelude hiding (lookup)

type Output      = [String]
type ErrorMessage = String

data RuntimeError =
    UnboundVariable VariableName
  | BadFunction     FunctionName
  | BadArgument     ErrorMessage
  deriving (Eq, Show)

type Environment = [(VariableName, Value)]
type Runtime a   = Environment -> (Either RuntimeError a, Output)

newtype Boa a = Boa { run :: Runtime a }

-- Applicative instance.
instance Applicative Boa where
  pure a = Boa (\env -> (Right a, []))
  (<*>) = ap

-- Monad instance.
instance Monad Boa where
  return = pure
  (Boa m) >>= f = Boa (\env ->
      let (res, out1) = m env in
        case res of
          Left err -> (Left err, out1)
          Right a  -> let (res2, out2) = run (f a) env
                      in (res2, out1 ++ out2))

instance Functor Boa where
  fmap = liftM

-- Operations of the Boa monad
abort :: RuntimeError -> Boa a
abort err = Boa (\_ -> (Left err, []))

look :: VariableName -> Boa Value
look x = Boa (\env -> case lookup' x env of
                          Just v  -> (Right v, [])
                          Nothing -> (Left (UnboundVariable x), []))
  where
    lookup' _ [] = Nothing
    lookup' key ((k,v):xs)
      | key == k  = Just v
      | otherwise = lookup' key xs

bind :: VariableName -> Value -> (Boa a -> Boa a)
bind x v b = Boa (\env -> run b ((x,v):env))

output :: String -> Boa ()
output s = Boa (\_ -> (Right (), [s]))

-- Helper functions for interpreter

truthy :: Value -> Bool
truthy None            = False
truthy (Boolean False) = False
truthy (Number 0)      = False
truthy (Text "")       = False
truthy (List [])       = False
truthy _               = True

operate :: OperationSymbol -> Value -> Value -> Either ErrorMessage Value
operate Plus (Number a) (Number b)    = Right $ Number (a + b)
operate Minus (Number a) (Number b)   = Right $ Number (a - b)
operate Times (Number a) (Number b)   = Right $ Number (a * b)
operate Div (Number a) (Number b)     = if b == 0 then Left "division by zero" else Right $ Number (a `div` b)
operate Mod (Number a) (Number b)     = if b == 0 then Left "modulo by zero" else Right $ Number (a `mod` b)
operate Eq v1 v2                      = Right $ Boolean (v1 == v2)
operate Less (Number a) (Number b)    = Right $ Boolean (a < b)
operate Greater (Number a) (Number b) = Right $ Boolean (a > b)
operate op _ _                      = Left ("invalid operands for operator " ++ show op)

valueToString :: Value -> String
valueToString None         = "None"
valueToString (Boolean b)  = if b then "True" else "False"
valueToString (Number n)   = show n
valueToString (Text s)     = s
valueToString (List xs)    = show xs

apply :: FunctionName -> [Value] -> Boa Value
apply "print" args = do
  let line = unwords (map valueToString args)
  output line
  return None
apply "range" args = case args of
  [Number n] ->
    return $ List [Number i | i <- [0..n-1]]
  [Number a, Number b] ->
    return $ List [Number i | i <- [a..b-1]]
  [Number a, Number b, Number step] ->
    if step == 0
      then abort (BadArgument "range step cannot be zero")
      else return $ List (map Number (takeWhile (\x -> if step > 0 then x < b else x > b)
                                      (iterate (+ step) a)))
  _ -> abort (BadArgument "range expects 1 to 3 integer arguments")
apply f _ = abort (BadFunction f)

-- Main functions of interpreter

eval :: Expression -> Boa Value
eval (Constant v) = return v
eval (Variable x) = look x
eval (Operation op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case operate op v1 v2 of
    Right value -> return value
    Left err    -> abort (BadArgument err)
eval (Not e) = do
  v <- eval e
  return $ Boolean (not (truthy v))
eval (Call f args) = do
  vs <- mapM eval args
  apply f vs
eval (ListExpression es) = do
  vs <- mapM eval es
  return $ List vs
eval (ListComprehension e0 clauses) = do
  results <- comp clauses
  return $ List results
  where
    comp [] = do
      v <- eval e0
      return [v]
    comp (cl:cls) = case cl of
      For x expr -> do
         listVal <- eval expr
         case listVal of
           List xs -> do
             lists <- mapM (\v -> bind x v (comp cls)) xs
             return (concat lists)
           _ -> abort (BadArgument "for clause expects a list")
      If expr -> do
         cond <- eval expr
         if truthy cond then comp cls else return []

exec :: Program -> Boa ()
exec [] = return ()
exec (Define x body : program) = do
  v <- eval body
  bind x v (exec program)
exec (Execute expr : program) = do
  _ <- eval expr
  exec program

execute :: Program -> (Output, Maybe RuntimeError)
execute p =
  let (result, out) = run (exec p) []
  in (out, case result of Left err -> Just err; Right _ -> Nothing)


module Syntax where

type VariableName = String
type FunctionName = String

data Value =
    None
  | Boolean Bool
  | Number  Integer
  | Text    String
  | List    [Value]
  deriving (Show, Read, Eq)

data OperationSymbol = Plus | Minus | Times | Div | Mod | Eq | Less | Greater
  deriving (Show, Read, Eq)

data Expression =
    Constant  Value
  | Variable  VariableName
  | Operation OperationSymbol Expression Expression
  | Not Expression
  | Call FunctionName [Expression]
  | ListExpression    [Expression]
  | ListComprehension Expression [Clause]
  deriving (Show, Read, Eq)

data Clause =
    For VariableName Expression
  | If Expression
  deriving (Show, Read, Eq)

data Statement =
    Define  VariableName Expression
  | Execute Expression
  deriving (Show, Read, Eq)

type Program = [Statement]
