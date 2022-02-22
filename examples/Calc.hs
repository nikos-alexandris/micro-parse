{-# OPTIONS -Wall #-}

module Calc where

import           Parser
import           System.IO (hFlush, stdout)

data UnaryOp
    = Plus
    | Minus
    deriving (Show)

data BinaryOp
    = Add
    | Sub
    | Mul
    | Div

data Expr
    = Number Double
    | Unary UnaryOp Expr
    | Binary Expr BinaryOp Expr
    | Paren Expr

instance Show BinaryOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

instance Show Expr where
    show (Number n)      = show n
    show (Unary op e)    = "(" ++ show op ++ show e ++ ")"
    show (Binary l op r) = "(" ++ show l ++ show op ++ show r ++ ")"
    show (Paren e)       = "(" ++ show e ++ ")"

calc :: IO ()
calc = do
    putStr "> " >> hFlush stdout
    str <- getLine
    case runParser pExpr str of
        Just (expr, "") -> print $ eval expr
        _               -> putStrLn "Could not parse expression"
    calc

repl :: IO ()
repl = do
    putStr "> "
    str <- getLine
    case runParser pExpr str of
        Just (expr, "") -> print $ eval expr
        _               -> putStrLn "Could not parse expression"

eval :: Expr -> Double
eval (Number n) = n
eval (Unary Plus r) = eval r
eval (Unary Minus r) = - eval r
eval (Binary l o r) = eval l `op` eval r
    where op =
            case o of
                Add -> (+)
                Sub -> (-)
                Mul -> (*)
                Div -> (/)
eval (Paren e) = eval e

pExpr :: Parser Expr
pExpr = pUnary <|> pAddition

pUnary :: Parser Expr
pUnary = do
    op <- Plus <$ char '+' <|> Minus <$ char '-'
    Unary op <$> pExpr

pAddition :: Parser Expr
pAddition = do
    l <- pProduct
    rest l <|> pure l
    where
        rest l' = do
            op <- Add <$ char '+' <|> Sub <$ char '-'
            r <- pProduct
            let l'' = Binary l' op r
            rest l'' <|> pure l''

pProduct :: Parser Expr
pProduct = do
    l <- pSimple
    rest l <|> pure l
    where
        rest l' = do
            op <- Mul <$ char '*' <|> Div <$ char '/'
            r <- pProduct
            let l'' = Binary l' op r
            rest l'' <|> pure l''

pSimple :: Parser Expr
pSimple = pNumber <|> pParen

pParen :: Parser Expr
pParen = Paren <$> (manySpaces *> char '(' *> pExpr <* char ')' <* manySpaces)

pNumber :: Parser Expr
pNumber = Number <$> (manySpaces *> uDouble <* manySpaces)

