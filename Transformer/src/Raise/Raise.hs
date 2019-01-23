{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Raise.Raise where

import Control.Monad
import Common.MonadRaise
import Common.Transformer

data Raise (m :: * -> *) a = Return a
                           | forall b. (Raise m b) :>>= (b -> Raise m a)
                           | Raise Exception
                           | Promote (m a)

instance Functor m => Functor (Raise m) where
    fmap :: (a -> b) -> Raise m a -> Raise m b
    fmap f (Return x)  = Return $ f x
    fmap _ (Raise e)   = Raise e
    fmap f (Promote m) = Promote $ f <$> m
    fmap f (m :>>= k)  = m :>>= \x -> f <$> k x
 
instance Applicative m => Applicative (Raise m) where
    pure :: a -> Raise m a
    pure = Return

    (<*>) :: Raise m (a -> b) -> Raise m a -> Raise m b
    Return f <*> m          = f <$> m
    Raise e <*> _           = Raise e
    _ <*> Raise e           = Raise e
    Promote m <*> Return x  = Promote $ m <*> (pure x)
    Promote m <*> Promote v = Promote $ m <*> v
    f <*> (v :>>= k)        = v :>>= \x -> f <*> (k x)
    (m :>>= f) <*> v        = m :>>= \x -> (f x) <*> v

instance Monad m => Monad (Raise m) where
    return :: a -> Raise m a
    return = Return

    (>>=) :: Raise m a -> (a -> Raise m b) -> Raise m b
    (>>=) = (:>>=)

    fail :: Exception -> Raise m a
    fail = Raise

instance Monad m => MonadRaise (Raise m) where
    raise :: Exception -> Raise m a
    raise = Raise

instance Transformer Raise where
    promote :: Monad m => m a -> Raise m a
    promote = Promote

    observe :: Monad m => Raise m a -> m a
    observe (Return a)  = return a
    observe (Raise e)   = fail e
    observe (Promote m) = m
    
    observe ((Return x)  :>>= k) = observe (k x)
    observe ((Raise e)   :>>= _) = fail e
    observe ((Promote m) :>>= k) = m >>= (\x -> observe (k x))
    observe ((m :>>= k)  :>>= t) = observe (m :>>= (\x -> k x :>>= t))  

instance (Show (m a), Monad m) => Show (Raise m a) where
    show = show . observe
