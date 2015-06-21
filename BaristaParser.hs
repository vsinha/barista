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

-- Match the lowercase or uppercase form of 'c'
--caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
--caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

-- TODO: parse so Special Words Start With Capital Letters
keyword :: Parser String
keyword = do c <- letter <|> char '_'
             cs <- many (letter <|> digit <|> char '_')
             return (c:cs)
       <?> "keyword" -- "this is the name to use when printing an error message

-- Parser combinator which skips whitespace from both sides
skipWhitespace :: Parser a -> Parser a
skipWhitespace p = ws *> p <* ws

-- Parser combinator: If one parser doesn't work, try the other
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

-- Parse a liquid measurement unit
measureP :: Parser Measure
measureP = string "ml" <||> string "mL" *> pure Milli

-- Parse the integer at the beginning of the line, if there is one
ingredientIndexP :: Parser (Maybe Int)
ingredientIndexP = Just <$> int
                  <|> (pure Nothing)

-- parses the arguments for the Hold action
holdP :: Parser Action
holdP = do 
      string "hold" <||> string "Hold"
      temp <- skipWhitespace int
      char '`' 
      unit <- choice [string "K" *> pure K
                     ,string "F" *> pure F
                     ,string "C" *> pure C] -- pure FriedChicken
      return $ Hold 5 C


-- both annotationP and actionP use actionItemP, which can be one of 
-- the keywords mix, mix_source, etc
actionItemP :: Parser Action
actionItemP = (choice . map try $ [string "mix_source" *> pure MixSource, 
                                   string "mix" *> pure Mix, 
                                   holdP,
                                   string "aspirate_speed" *> pure AspirateSpeed,
                                   string "dispense_speed" *> pure DispenseSpeed]) 

-- annotations go with ingredients
-- between square brackets [ ],
-- attempt to apply one or more of the parsers for Action types, 
-- each separated by a single comma an optional whitespace
annotationP :: Parser (Maybe [Action])
annotationP = Just <$> ((between (char '[') (char ']')) $
            sepEndBy actionItemP           
            (skipWhitespace $ char ','))
      <|> (pure Nothing)

ingredientLine :: Parser Ingredient
ingredientLine = do
       index <- ingredientIndexP
       char '-'
       volume <- skipWhitespace int
       char '`'
       measure <- skipWhitespace measureP
       ingredientName <- skipWhitespace keyword
       annotations <- annotationP
       eol
       return $ Ingredient volume measure ingredientName index annotations

-- actions go with recipe clauses
actionLine :: Parser Action
actionLine = do
      skipWhitespace . char $ '!'
      actionItem <- actionItemP
      eol
      return actionItem

recipeClause :: Parser RecipeClause
recipeClause = do
       ingredients <- many1 ingredientLine
       -- parse either a list of actions or return nothing
       actions <- (Just <$> many1 actionLine) <|> (pure Nothing)
       return $ RecipeClause ingredients actions

recipe :: Parser Recipe
recipe = do
       string "Recipe"
       title <- skipWhitespace keyword
       eol
       recipeClauses <- many1 recipeClause
       return $ Recipe title recipeClauses 
       
