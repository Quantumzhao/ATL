module Parser
(
  symbolChars
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
    MonadParsec(try) )
import Text.Megaparsec.Char ( char, space, string )
import Data.Void ( Void )
import Data.Char ( isAlphaNum )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)

type Parser = Parsec Void String

symbolChars :: Parser Char
symbolChars = satisfy $ \c -> isAlphaNum c || c  == '_'

parseValidSymbol :: [String] -> Parser String
parseValidSymbol symbol = choice (string <$> symbol) <* space
