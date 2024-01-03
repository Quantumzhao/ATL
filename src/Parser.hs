module Parser
(
  parseProgram
)
where

import Prelude hiding ((<>))
import Text.Megaparsec
    ( (<|>),
    empty,
    optional,
    anySingle,
    runParser,
    satisfy,
    choice,
    many,
    some,
    Parsec,
    try
    , eof )
import Text.Megaparsec.Char ( char, space, string )
import Data.Void ( Void )
import Data.Char ( isAlphaNum , isDigit )
import Interpreter.Types

type Parser = Parsec Void String

symbolChars :: Parser Char
symbolChars = satisfy $ \c -> isAlphaNum c || c  == '_'

digit :: Parser Char
digit = satisfy isDigit

parseValidSymbol :: [String] -> Parser String
parseValidSymbol symbol = choice (string <$> symbol) <* space

parseInt :: Parser Expr
parseInt = (Literal . Int . read) <$> ((++) <$> string "-" <*> some digit <|> some digit)

parseBool :: Parser Expr
parseBool = (Literal . Bool . read) <$> choice [string "true", string "false"]

colon :: Parser Char
colon = char ';'

parseUnit :: Parser Expr
parseUnit = string "()" *> (pure $ Literal Unit)

parseArray :: Parser Expr
parseArray = ArrayExpr <$> (char '[' *> many parseExpr <* char ']')
  where
  -- innerExprs = choice [pure, parseExpr]

parseParen :: Parser Expr
parseParen = char '(' *> parseExpr <* char ')'

parseRecord :: Parser Expr
parseRecord = undefined

parseVariable :: Parser Expr
parseVariable = undefined

parseCall :: Parser Expr
parseCall = undefined

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
parseAssign = undefined

parseBlock :: Parser [Statement]
parseBlock = char '{' *> parseStatements <* char '}'

parseIf :: Parser Statement
parseIf = do
  _ <- string "if"
  c <- parseParen
  t <- parseBlock
  f <- parseBlock
  return $ If c t f

parseWhile :: Parser Statement
parseWhile = do
  _ <- string "while"
  c <- parseParen
  l <- parseBlock
  return $ While c l

parseSwitch :: Parser Statement
parseSwitch = do
  _ <- string "switch"
  c <- parseParen
  undefined

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
