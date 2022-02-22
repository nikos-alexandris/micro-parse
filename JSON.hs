{-# OPTIONS -Wall #-}

module JSON where

import           Parser

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonString String
    | JsonNumber Integer
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

parseJsonFromFile :: FilePath -> IO (Maybe JsonValue)
parseJsonFromFile path = do
    file <- readFile path
    case runParser pJson file of
        Just (val, "") -> return $ Just val
        _              -> return Nothing

pJson :: Parser JsonValue
pJson =
    manySpaces *>
    (   pJsonNull
    <|> pJsonBool
    <|> pJsonString
    <|> pJsonNumber
    <|> pJsonArray
    <|> pJsonObject
    ) <* manySpaces

pJsonNull :: Parser JsonValue
pJsonNull = do
    _ <- string "null"
    return JsonNull

pJsonBool :: Parser JsonValue
pJsonBool = do
    s <- string "true" <|> string "false"
    return $
        case s of
            "true"  -> JsonBool True
            "false" -> JsonBool False
            _       -> undefined -- unreachable

pJsonString :: Parser JsonValue
pJsonString = JsonString <$> stringLiteral

pJsonNumber :: Parser JsonValue
pJsonNumber = JsonNumber <$> sint

pJsonArray :: Parser JsonValue
pJsonArray = do
    _ <- char '['
    vs <- sepManyBy (char ',') pJson
    _ <- char ']'
    return $ JsonArray vs

pJsonObject :: Parser JsonValue
pJsonObject = do
    _ <- char '{' *> manySpaces
    fields <- sepManyBy (char ',' *> manySpaces) pair
    _ <- char '}'
    return $ JsonObject fields
    where
        pair = do
            k <- stringLiteral *> manySpaces
            _ <- char ':'
            v <- pJson
            return (k, v)
