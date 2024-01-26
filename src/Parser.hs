module Parser
( parseProgram
)
where

import Prelude hiding ((<>))
import Text.Megaparsec
  ( (<|>)
  , satisfy
  , choice
  , many
  , some
  , Parsec
  , try
  , eof )
import Text.Megaparsec.Char ( char, string )
import Data.Void ( Void )
import Data.Char ( isAlphaNum , isDigit )
import Interpreter.Types
import Control.Monad ( void )

type Parser = Parsec Void String

symbolChars :: Parser Char
symbolChars = satisfy $ \c -> isAlphaNum c || c  == '_'

digit :: Parser Char
digit = satisfy isDigit

parseValidSymbol :: Parser String
parseValidSymbol = some symbolChars

parseInt :: Parser Expr
parseInt = (Literal . Int . read) <$> ((++) <$> string "-" <*> some digit <|> some digit)

parseBool :: Parser Expr
parseBool = (Literal . Bool . read) <$> choice [string "true", string "false"]

colon :: Parser Char
colon = char ';'

parseUnit :: Parser Expr
parseUnit = string "()" *> (pure $ Literal Unit)

parseManySep :: Parser a -> Parser [a]
parseManySep p = try (do
  x <- p
  xs <- many (char ',' *> p)
  return $ x : xs)
  <|> pure []

parseArray :: Parser Expr
parseArray = ArrayExpr <$> (char '[' *> parseManySep parseExpr <* char ']')

parseParen :: Parser Expr
parseParen = char '(' *> parseExpr <* char ')'

parseRecord :: Parser Expr
parseRecord = RecordExpr <$> (char '{' *> parseManySep parseKvp <* char '}')
  where
    parseKvp = do
      key <- parseValidSymbol
      void $ char '='
      value <- parseExpr
      return (key, value)

parseVariable :: Parser Expr
parseVariable = Variable <$> parseValidSymbol

parseCall :: Parser Expr
parseCall = do
  fname <- parseValidSymbol
  void $ char '('
  exprs <- parseManySep parseExpr
  void $ char ')'
  return $ Call fname exprs

parseExpr :: Parser Expr
parseExpr = choice
  [ try parseInt
  , try parseBool
  , try parseUnit
  , try parseArray
  , try parseRecord
  , try parseCall
  , try parseVariable ]

parseAssign :: Parser Statement
parseAssign = do
  void $ string "var"
  var <- parseValidSymbol
  void $ char '='
  expr <- parseExpr
  return $ AssignDefine var expr

parseBlock :: Parser [Statement]
parseBlock = char '{' *> parseStatements <* char '}'

parseIf :: Parser Statement
parseIf = do
  void $ string "if"
  c <- parseParen
  t <- parseBlock
  f <- parseBlock
  return $ If c t f

parseWhile :: Parser Statement
parseWhile = do
  void $ string "while"
  c <- parseParen
  l <- parseBlock
  return $ While c l

parseSwitch :: Parser Statement
parseSwitch = do
  void $ string "switch"
  c <- parseParen
  void $ char '{'
  cs <- parseCases
  void $ char '}'
  return $ Switch c cs
  where
    parseCases = undefined
    parseCase = undefined

parseReturnX :: Parser Statement
parseReturnX = ReturnX <$> (string "return" *> parseExpr <* colon)

parseReturn :: Parser Statement
parseReturn = string "return" *> pure Return <* colon

parseBreak :: Parser Statement
parseBreak = string "break" *> pure Break <* colon

parseImpure :: Parser Statement
parseImpure = Impure <$> parseExpr <* colon

parseStatement :: Parser Statement
parseStatement = choice 
  [ try parseAssign
  , try parseIf
  , try parseWhile
  , try parseSwitch
  , try parseReturnX
  , try parseReturn
  , try parseBreak
  , try parseImpure ]

parseStatements :: Parser [Statement]
parseStatements = many parseStatement

parseProgram :: Parser [Statement]
parseProgram = parseStatements <* eof
