{-# OPTIONS -Wall #-}

module Parser
  ( Parser(..)
  , module Control.Applicative
  , char
  , charPred
  , stringLiteral
  , string
  , uInt
  , sInt
  , uDouble
  , sDouble
  , sepSomeBy
  , sepManyBy
  , someSpaces
  , manySpaces
  ) where

import           Control.Applicative
import           Data.Char           (isDigit, isSpace)

newtype Parser a = Parser
    { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (x, input') <- p input
            return (f x, input')

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (f, input') <- p1 input
            (a, input'') <- p2 input'
            pure (f a, input'')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) =
         Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
    (Parser p) >>= f =
        Parser $ \input -> do
            (x, input') <- p input
            let Parser p' = f x
            p' input'

someSpaces :: Parser String
someSpaces = some (charPred isSpace)

manySpaces :: Parser String
manySpaces = many (charPred isSpace)

sepSomeBy :: Parser a -> Parser b -> Parser [b]
sepSomeBy sep el = do
    first <- el
    rest <- many $ sep *> el
    return $ first : rest

sepManyBy :: Parser a -> Parser b -> Parser [b]
sepManyBy sep el = sepSomeBy sep el <|> return []

uInt :: Parser Integer
uInt = do
    s <- some (charPred isDigit)
    return $ read s

sInt :: Parser Integer
sInt = do { s <- char '+' <|> char '-'; n <- uInt; case s of '+' -> return n; _ -> return (-n) }
   <|> uInt

uDouble :: Parser Double
uDouble = do
    d <- some (charPred isDigit)
    f <- (char '.' *> many (charPred isDigit)) <|> pure []
    case f of
        [] -> return $ read d
        _  -> return $ read (d ++ ('.' : f))

sDouble :: Parser Double
sDouble = do { s <- char '+' <|> char '-'; n <- uDouble; case s of '+' -> return n; _ -> return (-n) }
   <|> uDouble


string :: String -> Parser String
string = traverse char

stringLiteral :: Parser String
stringLiteral = do
    _ <- char '\"'
    s <- many (charPred (/= '\"'))
    _ <- char '\"'
    return s

char :: Char -> Parser Char
char c = charPred (== c)

charPred :: (Char -> Bool) -> Parser Char
charPred f = Parser $ \input ->
    case input of
        (x:xs) | f x -> Just (x, xs)
        _            -> Nothing
