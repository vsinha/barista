module BaristaParser where

import Text.ParserCombinators.Parsec
import Types
import Control.Applicative hiding ((<|>), optional, many)

-- White Space
ws :: Parser String
ws = many (oneOf " ")

-- EOL
eol :: Parser ()
eol = do many1 (oneOf "\n\r;")
         return ()
         <?> "end of line"

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
skipWhitespace :: Parser a -> Parser a
skipWhitespace p = ws *> p <* ws

-- If one parser doesn't work, try the other
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

-- Parse a measurement unit
measureP :: Parser String
measureP = string "ml" <||> string "mL" *> pure "ml"

ingredientIndexP :: Parser (Maybe Int)
ingredientIndexP = Just <$> int
                  <|> (pure Nothing)

-- attempt to apply one or more of the string parsers, each separated by a single comma
annotationP :: Parser (Maybe [Annotation])
annotationP = Just <$> ((between (char '[') (char ']')) $
            sepEndBy (choice [(string "mix"            *> pure Mix)
                             ,(string "mix_source"     *> pure MixSource)
                             ,(string "hold"           *> pure Hold)
                             ,(string "aspirate_speed" *> pure AspirateSpeed)
                             ,(string "dispense_speed" *> pure DispenseSpeed)]) 
      (skipWhitespace $ char ','))
      <|> (pure Nothing)

ingredient :: Parser Ingredient
ingredient = do
       index <- ingredientIndexP
       char '-'
       volume <- skipWhitespace int
       char '`'
       measure <- skipWhitespace measureP
       ingredientName <- skipWhitespace keyword
       annotations <- annotationP
       eol
       return $ Ingredient volume measure ingredientName index annotations

recipe :: Parser Recipe
recipe = do
       string "Recipe"
       title <- skipWhitespace keyword
       eol
       ingredients <- many1 ingredient
       return $ Recipe title ingredients
       
