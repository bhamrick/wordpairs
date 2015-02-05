{-# LANGUAGE LambdaCase #-}
module Data.ListParse where

import Control.Applicative
import Control.Monad

-- Quick and dirty monadic parser for list inputs

data Parser a r = Parser { parse :: [a] -> Either String ([a], r) }

instance Functor (Parser a) where
    fmap f (Parser p) = Parser (fmap (fmap f) . p)

instance Monad (Parser a) where
    return x = Parser $ \s -> Right (s, x)
    Parser p >>= f = Parser $ \s ->
        case p s of
            Left e -> Left e
            Right (s', r) -> parse (f r) s'

instance Applicative (Parser a) where
    pure = return
    (<*>) = ap

instance Alternative (Parser a) where
    empty = Parser $ const (Left "empty")
    Parser p1 <|> Parser p2 = Parser $ \s ->
        case p1 s of
            Left _ -> p2 s
            Right (s', r) -> Right (s', r)

token :: Eq a => a -> Parser a ()
token a = Parser $ \case
    t:ts -> if t == a
        then Right (ts, ())
        else Left "token mismatch"
    _ -> Left "No token to match"

anyToken :: Parser a a
anyToken = Parser $ \case
    [] -> Left "No token"
    t:ts -> Right (ts, t)

endOfInput :: Parser a ()
endOfInput = Parser $ \case
    [] -> Right ([], ())
    _ -> Left "Unexpected token at end of input"
