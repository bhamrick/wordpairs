{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Regex where

import Control.Applicative hiding (empty)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8

import Data.ByteString (ByteString)
import Data.FAlgebra

data RegexF a = Empty
              | CharSet [Char]
              | AntiCharSet [Char]
              | Star a
              | Plus a
              | OneOf a a
              | Sequence a a
    deriving (Eq, Show, Ord, Functor)

type Regex = Fix RegexF

star :: FAlgebra RegexF r => r -> r
star = alg . Star

plus :: FAlgebra RegexF r => r -> r
plus = alg . Plus

optional :: FAlgebra RegexF r => r -> r
optional = oneof empty

oneof :: FAlgebra RegexF r => r -> r -> r
oneof x y = alg (OneOf x y)

empty :: FAlgebra RegexF r => r
empty = alg Empty

sequence :: FAlgebra RegexF r => r -> r -> r
sequence x y = alg (Sequence x y)

charset :: FAlgebra RegexF r => [Char] -> r
charset = alg . CharSet

anticharset :: FAlgebra RegexF r => [Char] -> r
anticharset = alg . AntiCharSet

regexP :: Parser Regex
regexP = combine <$> (regexP' [])
    where
    combine :: [Regex] -> Regex
    combine [] = empty
    combine rs = foldr1 Data.Regex.sequence (reverse rs)
    regexP' :: [Regex] -> Parser [Regex]
    regexP' stack = foldr1 (<|>)
        [ endOfInput >> return stack
        , char ')' >> return stack
        , char '(' >> regexP >>= regexP' . (:stack)
        , char '*' >> case stack of
            [] -> fail "Lonely *"
            r:rs -> regexP' $ star r : rs
        , char '+' >> case stack of
            [] -> fail "Lonely +"
            r:rs -> regexP' $ plus r : rs
        , char '?' >> case stack of
            [] -> fail "Lonely ?"
            r:rs -> regexP' $ Data.Regex.optional r : rs
        , char '|' >> ((:[]) . oneof (combine stack) <$> regexP)
        , char '.' >> regexP' (anticharset [] : stack)
        , do
            string "[^"
            cs <- many (notChar ']')
            char ']'
            regexP' $ anticharset cs : stack
        , do
            char '['
            cs <- many (notChar ']')
            char ']'
            regexP' $ charset cs : stack
        , anyChar >>= \c -> regexP' (charset [c] : stack)
        ]

parseRegex :: ByteString -> Either String Regex
parseRegex = parseOnly regexP
