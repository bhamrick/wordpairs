{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.NFA where

import Control.Applicative hiding (empty)
import Control.Lens
import Control.Monad.State
import Data.FAlgebra
import Data.Function
import Data.List hiding (intersect)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void

import Data.Regex

ordNub :: Ord a => [a] -> [a]
ordNub = map head . group . sort

class PropLike a b | b -> a where
    prop :: b -> a -> Bool

instance PropLike a (a -> Bool) where
    prop = id

instance Ord a => PropLike a (Set a) where
    prop = flip Set.member

data NFA s p = NFA { _starts :: [s]
                   , _accepts :: [s]
                   , _immediates :: Map s [s]
                   , _consumers :: Map s [(s, p)]
                   }
    deriving (Eq, Show, Ord)

$(makeLenses ''NFA)

states :: Ord s => NFA s p -> [s]
states n = Set.toList . snd . flip runState Set.empty $ do
    forM (n ^. starts) $ \s -> do
        id %= Set.insert s
    forM (n ^. accepts) $ \s -> do
        id %= Set.insert s
    forM (n ^. immediates . _relations) $ \(s, t) -> do
        id %= Set.insert s
        id %= Set.insert t
    forM (n ^. consumers . _relations) $ \(s, (t, _)) -> do
        id %= Set.insert s
        id %= Set.insert t

intifyNFA :: Ord s => NFA s p -> NFA Int p
intifyNFA n = flip evalState (Map.empty, 0) $ do
    starts' <- each lookupRef (n ^. starts)
    accepts' <- each lookupRef (n ^. accepts)
    immediates' <- mapImmediatesM lookupRef (n ^. immediates)
    consumers' <- mapConsumersM lookupRef (n ^. consumers)
    return NFA
        { _starts = starts'
        , _accepts = accepts'
        , _immediates = immediates'
        , _consumers = consumers'
        }
    where
    lookupRef :: Ord s => s -> State (Map s Int, Int) Int
    lookupRef s = use (_1 . at s) >>= \case
        Nothing -> do
            r <- nextRef
            _1 %= Map.insert s r
            return r
        Just r -> return r
    nextRef :: Ord s => State (Map s Int, Int) Int
    nextRef = zoom _2 (id <<%= succ)

emptyNFA :: NFA () p
emptyNFA = NFA { _starts = [()]
               , _accepts = [()]
               , _immediates = Map.empty
               , _consumers = Map.empty
               }

nullNFA :: NFA Void p
nullNFA = NFA { _starts = []
              , _accepts = []
              , _immediates = Map.empty
              , _consumers = Map.empty
              }

transitionNFA :: p -> NFA Bool p
transitionNFA p = NFA
    { _starts = [False]
    , _accepts = [True]
    , _immediates = Map.empty
    , _consumers = Map.singleton False [(True, p)]
    }

makeMultiMap :: Ord a => [(a, b)] -> Map a [b]
makeMultiMap = foldr ins Map.empty
    where
    ins :: Ord a => (a, b) -> Map a [b] -> Map a [b]
    ins (a, b) = at a %~ \case
        Nothing -> Just [b]
        Just bs -> Just (b:bs)

fromMultiMap :: Map a [b] -> [(a, b)]
fromMultiMap m = [(a, b) | (a, bs) <- Map.toList m, b <- bs]

-- Lens for viewing a multimap as a list of pairs
_relations :: Ord k' => Lens (Map k [v]) (Map k' [v']) [(k, v)] [(k', v')]
_relations f = fmap makeMultiMap . f . fromMultiMap

-- Multimap traversals
_keys :: (Ord k', Applicative f) => (k -> f k') -> Map k [v] -> f (Map k' [v])
_keys = _relations . each . _1

_values :: (Ord k, Applicative f) => (v -> f v') -> Map k [v] -> f (Map k [v'])
_values = _relations . each . _2

mapImmediates :: Ord s' => (s -> s') -> Map s [s] -> Map s' [s']
mapImmediates f = over _values f . over _keys f

mapImmediatesM :: (Ord s', Applicative m, Monad m) => (s -> m s') -> Map s [s] -> m (Map s' [s'])
mapImmediatesM f = _values f <=< _keys f

mapConsumers :: Ord s' => (s -> s') -> Map s [(s, p)] -> Map s' [(s', p)]
mapConsumers f = over (_values . _1) f . over _keys f

mapConsumersM :: (Ord s', Applicative m, Monad m) => (s -> m s') -> Map s [(s, p)] -> m (Map s' [(s', p)])
mapConsumersM f = (_values . _1) f <=< _keys f

seqNFA :: (Ord s1, Ord s2) => NFA s1 p -> NFA s2 p -> NFA (Either s1 s2) p
seqNFA n1 n2 = NFA
        { _starts = Left <$> n1 ^. starts
        , _accepts = Right <$> n2 ^. accepts
        , _immediates = foldr (Map.unionWith (<>)) Map.empty
            [ mapImmediates Left (n1 ^. immediates)
            , mapImmediates Right (n2 ^. immediates)
            , Map.fromList [(Left a, Right <$> n2 ^. starts) | a <- n1 ^. accepts]
            ]
        , _consumers = Map.unionWith (<>) (mapConsumers Left $ n1 ^. consumers) (mapConsumers Right $ n2 ^. consumers)
        }

parNFA :: (Ord s1, Ord s2) => NFA s1 p -> NFA s2 p -> NFA (Either s1 s2) p
parNFA n1 n2 = NFA
        { _starts = (Left <$> n1 ^. starts) <> (Right <$> n2 ^. starts)
        , _accepts = (Left <$> n1 ^. accepts) <> (Right <$> n2 ^. accepts)
        , _immediates = Map.unionWith (<>) (mapImmediates Left $ n1 ^. immediates) (mapImmediates Right $ n2 ^. immediates)
        , _consumers = Map.unionWith (<>) (mapConsumers Left $ n1 ^. consumers) (mapConsumers Right $ n2 ^. consumers)
        }

instance Monoid (NFA Int p) where
    mempty = intifyNFA emptyNFA
    mappend n1 n2 = intifyNFA (seqNFA n1 n2)

(><) :: NFA Int p -> NFA Int p -> NFA Int p
n1 >< n2 = intifyNFA (parNFA n1 n2)

optionNFA :: NFA Int p -> NFA Int p
optionNFA = intifyNFA . parNFA emptyNFA

plusNFA :: Ord s => NFA s p -> NFA s p
plusNFA n = NFA
    { _starts = n ^. starts
    , _accepts = n ^. accepts 
    , _immediates = Map.unionWith (<>) (view immediates n) (Map.fromList [(a, view starts n) | a <- view accepts n])
    , _consumers = view consumers n
    }

starNFA :: NFA Int p -> NFA Int p
starNFA = optionNFA . plusNFA

opNFA :: Ord s => NFA s p -> NFA s p
opNFA n = NFA
    { _starts = n ^. accepts
    , _accepts = n ^. starts
    , _immediates = makeMultiMap [(t, s) | (s, t) <- fromMultiMap (n ^. immediates)]
    , _consumers = makeMultiMap [(t, (s, p)) | (s, (t, p)) <- fromMultiMap (n ^. consumers)]
    }

instance FAlgebra RegexF (NFA Int (Set Char)) where
    alg Empty = intifyNFA emptyNFA
    alg (CharSet cs) = intifyNFA . transitionNFA . Set.fromList $ cs
    alg (Star b) = starNFA b
    alg (Plus b) = plusNFA b
    alg (OneOf b1 b2) = b1 >< b2
    alg (Sequence b1 b2) = b1 <> b2
    alg (AntiCharSet cs) = intifyNFA . transitionNFA $ Set.fromList (map toEnum [0..127]) `Set.difference` Set.fromList cs

expandImmediates :: Ord s => NFA s p -> [s] -> [s]
expandImmediates n ss = expandImmediates' (Set.fromList ss) ss
    where
    expandImmediates' acc [] = Set.toList acc
    expandImmediates' acc (s:ss) =
        case n ^. immediates . at s of
            Nothing -> expandImmediates' acc ss
            Just states ->
                let states' = filter (flip Set.notMember acc) states
                    acc' = foldr Set.insert acc states'
                in expandImmediates' acc' (states' <> ss)

nfaTransition :: (Ord s, PropLike a p) => NFA s p -> a -> [s] -> [s]
nfaTransition n a = expandImmediates n . consume n a
    where
    consume :: (Ord s, PropLike a p) => NFA s p -> a -> [s] -> [s]
    consume n a ss = ordNub $ do
        s <- ss
        case n ^. consumers . at s of
            Nothing -> []
            Just ts -> ordNub [t | (t, p) <- ts, prop p a]

startNFA :: Ord s => NFA s p -> [s]
startNFA = expandImmediates <*> view starts

runNFA :: (Ord s, PropLike a p) => NFA s p -> [a] -> [s]
runNFA n as = runNFA' n as (startNFA n)
    where
    runNFA' :: (Ord s, PropLike a p) => NFA s p -> [a] -> [s] -> [s]
    runNFA' n [] states = states
    runNFA' n (a:as) states = runNFA' n as (nfaTransition n a states)

acceptsNFA :: (Ord s, PropLike a p) => NFA s p -> [a] -> Bool
acceptsNFA n as = not (Set.null (Set.intersection accepting final))
    where
    accepting = Set.fromList (n ^. accepts)
    final = Set.fromList (runNFA n as)

regexNFA :: FAlgebra RegexF (NFA Int p) => Regex -> NFA Int p
regexNFA = cata

data EitherSet a = Inside (Set a) | Outside (Set a)
    deriving (Eq, Show, Ord)

instance Ord a => PropLike a (EitherSet a) where
    prop (Inside s) = flip Set.member s
    prop (Outside s) = flip Set.notMember s

instance FAlgebra RegexF (NFA Int (EitherSet Char)) where
    alg Empty = intifyNFA emptyNFA
    alg (CharSet cs) = intifyNFA . transitionNFA . Inside . Set.fromList $ cs
    alg (AntiCharSet cs) = intifyNFA . transitionNFA . Outside . Set.fromList $ cs
    alg (Star b) = starNFA b
    alg (Plus b) = plusNFA b
    alg (OneOf b1 b2) = b1 >< b2
    alg (Sequence b1 b2) = b1 <> b2

class Intersectable a where
    intersect :: a -> a -> a

instance Intersectable (a -> Bool) where
    intersect f g x = f x && g x

instance Ord a => Intersectable (Set a) where
    intersect = Set.intersection

instance Ord a => Intersectable (EitherSet a) where
    intersect (Inside s1) (Inside s2) = Inside (Set.intersection s1 s2)
    intersect (Inside s1) (Outside s2) = Inside (Set.difference s1 s2)
    intersect (Outside s1) (Inside s2) = Inside (Set.difference s2 s1)
    intersect (Outside s1) (Outside s2) = Outside (Set.union s1 s2)

intersectNFA :: (Ord s1, Ord s2, Intersectable p) => NFA s1 p -> NFA s2 p -> NFA (s1, s2) p
intersectNFA n1 n2 = NFA
    { _starts = (,) <$> n1 ^. starts <*> n2 ^. starts
    , _accepts = (,) <$> n1 ^. accepts <*> n2 ^. accepts
    , _immediates = makeMultiMap $ [ ((s, x), (t, x))
                                   | (s, t) <- n1 ^. immediates . _relations
                                   , x <- states n2
                                   ]
                                <> [ ((x, s), (x, t))
                                   | x <- states n1
                                   , (s, t) <- n2 ^. immediates . _relations
                                   ]
    , _consumers = makeMultiMap [ ((s1, s2), ((t1, t2), intersect p1 p2))
                                | (s1, (t1, p1)) <- n1 ^. consumers . _relations
                                , (s2, (t2, p2)) <- n2 ^. consumers . _relations
                                ]
    }

instance Intersectable p => Intersectable (NFA Int p) where
    intersect n1 n2 = intifyNFA (intersectNFA n1 n2)
