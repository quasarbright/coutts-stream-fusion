{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Lib where

import Data.Function (on)

-- https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.104.7401&rep=rep1&type=pdf

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Stream a where
    Stream :: forall s a . (s -> Step s a) -> s -> Stream a
data Step s a = Done | Skip s | Yield a s deriving(Functor)

data List a = Nil | Cons a (List a)

instance Functor List where
    fmap f = lift $ mapS f

instance Foldable List where
    foldr f z = foldrS f z . stream

instance Applicative List where
    pure = return'
    fs <*> xs = concatMap' fs $ \f -> fmap f xs

instance Monad List where
    return = pure
    (>>=) = concatMap'

stream :: List a -> Stream a
stream = Stream next
    where
        next Nil = Done
        next (Cons x xs') = Yield x xs'
{-# INLINE [0] stream #-}

unstream :: Stream a -> List a
unstream (Stream next s0) = case next s0 of
    Done -> Nil
    Skip s -> unstream (Stream next s)
    Yield a s -> Cons a (unstream (Stream next s))
{-# INLINE [0] unstream #-}

{-# RULES
"stream/unstream" forall (s :: Stream a). stream (unstream s) = s
  #-}

lift :: (Stream a1 -> Stream a2) -> List a1 -> List a2
lift f = unstream . f . stream
{-# INLINE [0] lift #-}

mapS :: (a -> b) -> Stream a -> Stream b
mapS f (Stream k s) = Stream (fmap f . k) s

foldrS :: (a -> b -> b) -> b -> Stream a -> b
foldrS f z (Stream next s0) = go s0 where
    go s = case next s of
        Done -> z
        Skip s' -> go s'
        Yield a s' -> f a (go s')

filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (Stream next s0) = Stream next' s0 where
    next' s = case next s of
      Done -> Done
      Skip s' -> Skip s'
      Yield a s'
        | p a -> Yield a s'
        | otherwise -> Skip s'

filter' :: (a2 -> Bool) -> List a2 -> List a2
filter' = lift . filterS

returnS :: a -> Stream a
returnS a = Stream next True where
    next True = Yield a False
    next False = Done

return' :: a -> List a
return' = unstream . returnS

emptyS :: Stream a
emptyS = Stream (const Done) ()

empty' :: List a
empty' = Nil

appendS :: Stream a -> Stream a -> Stream a
appendS (Stream next s0) (Stream next' s0') = Stream next'' (Left s0) where
    next'' (Left s) = case next s of
      Done -> Skip (Right s0')
      Skip s' -> Skip (Left s')
      Yield a s' -> Yield a (Left s')
    next'' (Right s) = case next' s of
      Done -> Done
      Skip s' -> Skip (Right s')
      Yield a s' -> Yield a (Right s')

append' :: List a -> List a -> List a
append' a b = unstream $ (appendS `on` stream) a b

concatS :: Stream (Stream a) -> Stream a
concatS = foldrS appendS emptyS

concatMapS :: Stream a -> (a -> Stream b) -> Stream b
concatMapS (Stream next s0) k = Stream next' (s0, Nothing) where
    next' (s, Nothing) = case next s of
      Done -> Done
      Skip s' -> Skip (s', Nothing)
      Yield a s' -> Skip (s', Just (k a))
    next' (s, Just (Stream nextb sb)) = case nextb sb of
      Done -> Skip (s, Nothing)
      Skip sb' -> Skip (s, Just (Stream nextb sb'))
      Yield b sb' -> Yield b (s, Just (Stream nextb sb'))

concatMap' :: List a -> (a -> List b) -> List b
concatMap' xs k = unstream (concatMapS (stream xs) (stream . k))

-- TODO inline everything
