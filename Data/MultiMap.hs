module Data.MultiMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type MultiMap k v = Map k [v]

lookup :: Ord k => k -> MultiMap k v -> [v]
lookup k = fromMaybe [] . Map.lookup k

insert :: Ord k => k -> v -> MultiMap k v -> MultiMap k v
insert k v m = case Map.lookup k m of
    Nothing -> Map.insert k [v] m
    Just vs -> Map.insert k (v:vs) m
