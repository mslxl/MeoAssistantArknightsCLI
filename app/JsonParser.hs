module JsonParser where

import Control.Applicative
import Control.Monad (join)
import Data.Char
import Text.ParserCombinators.ReadP (satisfy)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonInteger Integer
  | JsonFloat Double
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (input', a) <- runParser p input
    Just (input', f a)

instance Applicative Parser where -- I'm not sure whether it satisfy Applicative 's rules in this impl way, but I can have a try.
  pure x = Parser $ \input -> Just (input, x)
  (Parser f1) <*> (Parser f2) = Parser $ \input -> do
    (input', f) <- f1 input
    (input'', a) <- f2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser f1) <|> (Parser f2) = Parser $ \input -> f1 input <|> f2 input

jsonValue :: Parser JsonValue
jsonValue = foldl1 (<|>) parsers
  where
    parsers = [jsonNull, jsonBool, jsonString, jsonNum, jsonArray]

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = str2bool <$> (stringP "true" <|> stringP "false")
  where
    str2bool "true" = JsonBool True
    str2bool "false" = JsonBool False
    str2bool _ = error "This should never happen"

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> elements <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> elements <* charP '}')
  where
    pairs = (,) <$> stringLiteral <*> (ws *> charP ':' *> ws *> jsonValue)
    elements = sepBy (ws *> charP ',' <* ws) pairs

jsonNum :: Parser JsonValue
jsonNum = Parser $ \input -> do
  -- It's so ugly. How to make it elegant?
  (input', beforeDot) <- runParser digitP input
  (input'', wholeNum) <-
    if not (null input') && head input' == '.'
      then runParser ((++) (beforeDot ++ ".") <$> digitP) $ tail input'
      else Just (input', beforeDot)
  case wholeNum of
    [] -> Nothing
    strs ->
      if '.' `elem` strs
        then Just (input'', JsonFloat $ read strs)
        else Just (input'', JsonInteger $ read strs)

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

stringLiteral :: Parser String
stringLiteral = charP '\"' *> (concat <$> stringLiteral') <* charP '\"'
  where
    stringLiteral' = many (escapeN <|> escapeR <|> escapeQuotation <|> escapeAs <|> normalStr) -- NOTE: It's not completed yet
    escapeAs = "\\" <$ stringP "\\\\"
    escapeN = "\n" <$ stringP "\\n"
    escapeR = "\r" <$ stringP "\\r"
    escapeQuotation = "\"" <$ stringP "\\\""
    normalStr = Parser $ \input -> case input of
      ('\"' : ys) -> Nothing -- fail the string and try others
      ('\\' : ys) -> Nothing
      (y : ys) -> Just (ys, [y])
      [] -> Nothing

charP :: Char -> Parser Char
charP c = Parser f
  where
    f [] = Nothing
    f (y : ys)
      | y == c = Just (ys, c)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP cond = Parser $ \input -> do
  let (a, b) = span cond input
  Just (b, a)

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

digitP :: Parser String
digitP = spanP isDigit
