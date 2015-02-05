{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Lens
import Data.ByteString.Char8 as BS
import Data.Foldable
import Data.ListParse
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.MultiMap as MultiMap
import Data.NFA
import Data.Regex
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

import Prelude as P

matchWordPairs :: forall s p. (PropLike Char p, Ord s) => [ByteString] -> NFA s p -> [(ByteString, ByteString)]
matchWordPairs wlist n = go wlist Map.empty Map.empty Set.empty
    where
    go :: [ByteString] -> Map s [ByteString] -> Map s [ByteString] -> Set (ByteString, ByteString) -> [(ByteString, ByteString)]
    go [] _ _ _ = []
    go (w:ws) forwards backwards results = let
        fstates = runNFA n (unpack w)
        bstates = runNFA opN (unpack . BS.reverse $ w)
        fcollisions = fstates >>= flip MultiMap.lookup backwards
        forwards' = P.foldr (flip MultiMap.insert w) forwards fstates
        bcollisions = bstates >>= flip MultiMap.lookup forwards'
        backwards' = P.foldr (flip MultiMap.insert w) backwards bstates
        collisionPairs = P.map ((,) w) fcollisions <> P.map (flip (,) w) bcollisions
        (newResults, results') = filterAndAdd collisionPairs results
        in
        newResults ++ go ws forwards' backwards' results'
    opN :: NFA s p
    opN = opNFA n
    filterAndAdd :: Ord a => [a] -> Set a -> ([a], Set a)
    filterAndAdd [] s = ([], s)
    filterAndAdd (a:as) s = if Set.member a s
        then filterAndAdd as s
        else over _1 (a:) (filterAndAdd as (Set.insert a s))

data Combined a = Simple a
                | Intersection (Combined a) (Combined a)
                | Union (Combined a) (Combined a)
    deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

combineNFA :: Intersectable p => Combined (NFA Int p) -> NFA Int p
combineNFA (Simple n) = n
combineNFA (Intersection n1 n2) = intersect (combineNFA n1) (combineNFA n2)
combineNFA (Union n1 n2) = combineNFA n1 >< combineNFA n2

combinedRegexNFA :: Combined Regex -> NFA Int (EitherSet Char)
combinedRegexNFA = combineNFA . fmap regexNFA

combinedP :: Parser String (Combined String)
combinedP = anyToken >>= \case
    "-and" -> Intersection <$> combinedP <*> combinedP
    "-or" -> Union <$> combinedP <*> combinedP
    t -> pure (Simple t)

parseQuery :: [String] -> Either String (Combined Regex)
parseQuery = (>>= traverse (parseRegex . BS.pack)) . fmap snd . parse (combinedP <* endOfInput)

main :: IO ()
main = do
    args <- getArgs
    case parseQuery args of
        Left e -> P.putStrLn e
        Right cr -> do
            let dictionaryFile = "/home/bhamrick/wordlists/wordlist.2e5"
            words <- BS.lines <$> BS.readFile dictionaryFile
            let nfa = combinedRegexNFA cr
            BS.putStr . BS.unlines . P.map (\(x, y) -> x <> BS.pack " " <> y) $ matchWordPairs words nfa
