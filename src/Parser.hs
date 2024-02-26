{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Parser
( 
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
import Types
import Control.Monad ( void )
import Data.Functor ( ($>) )

type Parser = Parsec Void String

symbolChars :: Parser Char
symbolChars = satisfy $ \c -> isAlphaNum c || c  == '_'

digit :: Parser Char
digit = satisfy isDigit

parseValidSymbol :: Parser String
parseValidSymbol = some symbolChars

parseInt :: Parser Expr
parseInt = XInt . read <$> ((++) <$> string "-" <*> some digit <|> some digit)

parseBool :: Parser Expr
parseBool = XBool . read <$> choice [string "true", string "false"]

semicolon :: Parser Char
semicolon = char ';'

-- parseUnit :: Parser Expr
-- parseUnit = string "()" $> XUnit

parseManySep :: Parser a -> Parser [a]
parseManySep p = try (do
  x <- p
  xs <- many (char ',' *> p)
  return $ x : xs)
  <|> pure []

parseArray :: Parser Expr
parseArray = XArray <$> (char '[' *> parseManySep parseExpr <* char ']')

parseParen :: Parser Expr
parseParen = char '(' *> parseExpr <* char ')'

parseRecord :: Parser Expr
parseRecord = XStruct <$> (char '{' *> parseManySep parseKvp <* char '}')
  where
    parseKvp = do
      key <- parseValidSymbol
      void $ char '='
      value <- parseExpr
      return (key, value)

parseVariable :: Parser Expr
parseVariable = XVar <$> parseValidSymbol

parseCall :: Parser Expr
parseCall = do
  fname <- parseValidSymbol
  void $ char '('
  exprs <- parseManySep parseExpr
  void $ char ')'
  return $ XCall fname exprs

parseExpr :: Parser Expr
parseExpr = choice
  [ try parseInt
  , try parseBool
  -- , try parseUnit
  , try parseArray
  , try parseRecord
  , try parseCall
  , try parseVariable ]

-- parseAssign :: Parser Statement
-- parseAssign = do
--   void $ string "var"
--   var <- parseValidSymbol
--   void $ char '='
--   AssignDefine var <$> parseExpr

-- parseBlock :: Parser [Statement]
-- parseBlock = char '{' *> parseStatements <* char '}'

-- parseIf :: Parser Statement
-- parseIf = do
--   void $ string "if"
--   c <- parseParen
--   t <- parseBlock
--   If c t <$> parseBlock

-- parseWhile :: Parser Statement
-- parseWhile = do
--   void $ string "while"
--   c <- parseParen
--   While c <$> parseBlock

-- parseSwitch :: Parser Statement
-- parseSwitch = do
--   void $ string "switch"
--   c <- parseParen
--   void $ char '{'
--   cs <- parseCases
--   void $ char '}'
--   return $ Switch c cs
--   where
--     parseCases = undefined
--     parseCase = undefined

-- parseReturnX :: Parser Statement
-- parseReturnX = ReturnX <$> (string "return" *> parseExpr <* colon)

-- parseReturn :: Parser Statement
-- parseReturn = (string "return" $> Return) <* colon

-- parseBreak :: Parser Statement
-- parseBreak = (string "break" $> Break) <* colon

-- parseImpure :: Parser Statement
-- parseImpure = Impure <$> parseExpr <* colon

-- parseStatement :: Parser Statement
-- parseStatement = choice
--   [ try parseAssign
--   , try parseIf
--   , try parseWhile
--   , try parseSwitch
--   , try parseReturnX
--   , try parseReturn
--   , try parseBreak
--   , try parseImpure ]

-- parseStatements :: Parser [Statement]
-- parseStatements = many parseStatement

-- parseProgram :: Parser [Statement]
-- parseProgram = parseStatements <* eof
