module BaristaParser where

import Text.ParserCombinators.Parsec
import Types
import Control.Applicative hiding ((<|>), optional, many)

-- White Space
ws :: Parser String
ws = many (oneOf " ")

-- Integrals
int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit

-- Quoted strings
stringLike :: Parser String
stringLike = char '"' *> many (noneOf ['\"', '\r', '\n']) <* char '"'

-- TODO: parse so Special Words Start With Capital Letters
keyword :: Parser String
keyword = do c <- letter <|> char '_'
             cs <- many (letter <|> digit <|> char '_')
             return (c:cs)
       <?> "keyword" -- "this is the name to use when printing an error message

-- Parser combinator which skips whitespace from both sides
lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

-- If one parser doesn't work, try the other
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

-- Parse a measurement unit
measureP :: Parser String
measureP = (string "ml" *> (pure "ml")) -- (TODO how to compress these into 1 line)
      <||> (string "mL" *> (pure "ml")) -- both capital and lower case "L" parse to "ml"

-- ss is short for syntactic sugar!
parseString :: String -> Parser (Maybe String)
parseString s = (string s *> (pure . Just $ s)) <|> pure Nothing

ingredient :: Parser Ingredient
ingredient = do
       lexeme (parseString "- ")
       volume <- lexeme int
       lexeme (parseString "`")
       measure <- lexeme measureP
       ingredientName <- lexeme keyword
       string "\r\n"
       return $ Ingredient volume measure ingredientName

recipe :: Parser Recipe
recipe = do
       lexeme (parseString "Recipe")
       title <- lexeme keyword
       string "\r\n"
       ingredients <- many1 ingredient
       return $ Recipe title ingredients
       


