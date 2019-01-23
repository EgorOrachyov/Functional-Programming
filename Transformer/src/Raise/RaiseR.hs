{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Raise.RaiseR where

import Data.Kind
import Control.Monad
import Common.MonadRaise
import Common.Transformer

data RaiseR (m :: * -> *) a = Raise Exception
                            | forall b. (RaiseR m b) :>>= (b -> RaiseR m a)
                            | Promote (m a)

instance Functor m => Functor (RaiseR m) where
    fmap :: (a -> b) -> RaiseR m a -> RaiseR m b
    fmap _ (Raise e)          = Raise e
    fmap f (Promote m)        = Promote $ f <$> m
    fmap f (Raise e :>>= _)   = Raise e
    fmap f (Promote m :>>= k) = Promote m :>>= (\b -> f <$> (k b))

instance Applicative m => Applicative (RaiseR m) where
    pure :: a -> RaiseR m a
    pure = Promote . pure

    (<*>) :: RaiseR m (a -> b) -> RaiseR m a -> RaiseR m b
    Raise e <*> _           = Raise e
    _ <*> Raise e           = Raise e
    Promote f <*> Promote m = Promote $ f <*> m
    f <*> (v :>>= k)        = v :>>= (\x -> f <*> k x)
    (m :>>= k) <*> v        = m :>>= (\x -> (k x) <*> v)

instance Monad m => Monad (RaiseR m) where
    return :: a -> RaiseR m a
    return = pure

    (>>=) :: RaiseR m a -> (a -> RaiseR m b) -> RaiseR m b
    (>>=) = (:>>=)

    fail :: Exception -> RaiseR m a
    fail = Raise

instance Monad m => MonadRaise (RaiseR m) where
    raise :: Exception -> RaiseR m a
    raise = fail

instance Transformer RaiseR where
    promote :: Monad m => m a -> RaiseR m a
    promote = Promote

    observe :: Monad m => RaiseR m a -> m a
    observe (Raise e)           = fail e
    observe (Promote m)         = m
    observe (Promote m :>>= k)  = m >>= \x -> observe $ k x
    observe (Raise e :>>= _)    = fail e
    observe ((m :>>= k) :>>= f) = observe $ m :>>= (\x -> (k x) :>>= f)

instance (Show (m a), Monad m) => Show (RaiseR m a) where
    show = show . observe
