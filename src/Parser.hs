{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser
  ( parse
  , parseProgram
  )
where

import Text.Megaparsec
  ( (<|>)
  , satisfy
  , choice
  , many
  , some
  , Parsec
  , try
  , eof
  , runParser )
import Text.Megaparsec.Char ( char, string, newline )
import Data.Void ( Void )
import Data.Char ( isAlphaNum , isDigit )
import Types
import Control.Monad ( void )
import Data.Functor ( ($>) )
import Control.Monad.Combinators.Expr ( makeExprParser, Operator (..) )
import Text.Megaparsec.Char.Lexer ( symbol )

type Parser = Parsec Void String
data Quad = QProj Variable
          | QIndex Expr
          | QBinop (Expr -> Expr -> Expr) Expr
          | QTerm

symbolChars :: Parser Char
symbolChars = satisfy $ \c -> isAlphaNum c || c  == '_'

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = char ' ' <|> newline

(|>) :: Parser a -> Parser b -> Parser b
(|>) a b = a Prelude.*> many space Prelude.*> b
(<|) :: Parser a -> Parser b -> Parser a
(<|) a b = a <* many space <* b

parseValidSymbol :: Parser String
parseValidSymbol = some symbolChars

parseInt :: Parser Expr
parseInt = XInt . read <$> ((++) <$> string "-" <*> some digit <|> some digit)

parseBool :: Parser Expr
parseBool = XBool . read <$> (string "true" <|> string "false")

end :: Parser Char
end = char ';' <* many space

parseUnit :: Parser Expr
parseUnit = string "()" $> XUnit

parseManySep :: Parser a -> Parser [a]
parseManySep p = try (do
  x <- p
  xs <- many (char ',' |> p)
  return $ x : xs)
  <|> pure []

parseArray :: Parser Expr
parseArray = XArray <$> (char '[' |> parseManySep parseExpr <| char ']')

parseParen :: Parser Expr
parseParen = char '(' |> parseExpr <| char ')'

parseRecord :: Parser Expr
parseRecord = XStruct <$> (char '{' |> parseManySep parseKvp <| char '}')
  where
    parseKvp = do
      key <- parseValidSymbol <| char ':' <* many space
      value <- parseExpr
      return (key, value)

parseProj :: Parser Quad
parseProj = QProj <$> char '.' |> parseValidSymbol
parseIndex :: Parser Quad
parseIndex = QIndex <$> char '[' |> parseExpr <| char ']'

parseCall :: Parser Expr
parseCall = do
  fname <- parseValidSymbol
  void $ char '('
  exprs <- parseManySep parseExpr
  void $ char ')'
  return $ XCall fname exprs

parseTerm :: Parser Expr
parseTerm = choice
  [ try parseInt
  , try parseBool
  , try parseUnit
  , try $ XSymbol <$> parseValidSymbol
  , try parseArray
  , try parseRecord
  , try parseParen
  , try parseCall
  , try $ char '!' |> parseExpr ]

parseBinop :: Parser Quad
parseBinop = choice
  [ try $ parseBinop' "+" XAdd
  , try $ parseBinop' "-" XSub
  , try $ parseBinop' ">" XGt
  , try $ parseBinop' "<" XLt
  , try $ parseBinop' "==" XEqual
  , try $ parseBinop' "&&" XAnd
  , try $ parseBinop' "||" XOr ]
  where
    parseBinop' :: String -> (Expr -> Expr -> Expr) -> Parser Quad
    parseBinop' sym ctor = QBinop ctor <$> (string sym |> parseExpr)

parseExpr :: Parser Expr
parseExpr = do
  first <- parseTerm
  res <- try parseBinop <|> try parseIndex <|> try parseProj <|> pure QTerm
  case res of
    QBinop ctor second -> return $ ctor first second
    QIndex i -> return $ XIndex first i
    QProj key -> return $ XProj first key
    QTerm -> return first

parseAssign :: Parser Statement
parseAssign = do
  var <- parseValidSymbol <* many space
  next <- many (try parseProj' <|> try parseIndex') <| char '='
  let lhs = foldl (\a x -> case x of
          Left key -> VProj a key
          Right i -> VIndex a i
        ) (VSymbol var) next
  rhs <- many space *> parseExpr
  return (SAssign lhs rhs) <| end
  where
    parseProj' = Left <$> char '.' |> parseValidSymbol
    parseIndex' = Right <$> char '[' |> parseExpr <| char ']'

parseBlock :: Parser [Statement]
parseBlock = char '{' |> parseStatements <| char '}'

parseIf :: Parser Statement
parseIf = do
  void $ many space
  c <- string "if" |> parseParen
  void $ many space
  t <- parseBlock
  void $ many space
  f <- string "else" |> parseBlock
  SIf c t <$> parseBlock

parseWhile :: Parser Statement
parseWhile = do
  c <- string "while" |> parseParen
  void $ many space
  SWhile c <$> parseBlock

parseSwitch :: Parser Statement
parseSwitch = do
  void $ many space
  e <- string "switch" |> char '(' |> parseExpr <| char ')'
  void $ many space
  cs <- char '{' |> parseCases <| char '}'
  return $ SSwitch e cs
  where
    parseCases = try (do
      x <- parseCase
      xs <- many (many space *> parseCase)
      return $ x : xs)
      <|> pure []
    parseCase = do
      p <- string "case" |> parsePattern <| char ':'
      void $ many space
      c <- parseStatements <| string "break;"
      void $ many space
      return (p, c)
    parsePattern = choice [ try $ PValue <$> parseValueP
                          , try $ PArray <$> parseArrayP
                          , try $ PStruct <$> parseStructP
                          , char '_' $> PWildcard ]
    parseValueP = try (MatchV <$> parseExpr) <|> (BindV <$> parseValidSymbol)
    parseArrayP = choice
      [ try $ string "[]" $> EmptyA
      , try $ SkipSomeA <$> (char ',' |> string "..." |> char ',' |> parseArrayP)
      , CheckA <$> (parsePattern <| char ',') <*> parseArrayP ]
    parseStructP = many ((,) <$> parseValidSymbol <*> parsePattern)

parseReturn :: Parser Statement
parseReturn = SReturn <$> (string "return" |> parseExpr <| end)

parseBreak :: Parser Statement
parseBreak = string "break" <| end $> SBreak

parseImpure :: Parser Statement
parseImpure = SImpure <$> parseExpr <| end

parseProcedure :: Parser Statement
parseProcedure = do
  void $ many space
  fname <- parseValidSymbol <| char '('
  ps <- parseManySep parseValidSymbol <| char ')'
  void $ many space
  body <- parseBlock
  return $ SProcedure (fname, ps, body)

parseStatement :: Parser Statement
parseStatement = choice
  [ try parseReturn
  , try parseBreak
  , try parseIf
  , try parseWhile
  , try parseSwitch
  , try parseProcedure
  , try parseAssign
  , try parseImpure ]

parseStatements :: Parser [Statement]
parseStatements = many parseStatement

parseProgram :: Parser [Statement]
parseProgram = parseStatements <| eof

parse :: String -> Either String Program
parse s = case runParser parseProgram "" s of
  Right r -> return r
  Left l -> Left $ show l
