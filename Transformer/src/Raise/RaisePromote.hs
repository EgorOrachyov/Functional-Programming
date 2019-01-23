{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Raise.RaisePromote where

import Control.Monad
import Common.MonadRaise
import Common.Transformer

data RaisePromote (m :: * -> *) a = Return a
                                  | forall b. (m b) :>>- (b -> RaisePromote m a)
                                  | RaisePromote Exception

instance Functor m =>  Functor (RaisePromote m) where
    fmap :: (a -> b) -> RaisePromote m a -> RaisePromote m b
    fmap f (Return x) = Return $ f x
    fmap _ (RaisePromote e) = RaisePromote e
    fmap f (m :>>- k) = m :>>- \x -> f <$> (k x) 

instance Monad m => Applicative (RaisePromote m) where
    pure :: a -> RaisePromote m a
    pure = Return

    (<*>) :: RaisePromote m (a -> b) -> RaisePromote m a -> RaisePromote m b
    fs <*> xs = do {
        f <- fs;
        x <- xs;
        return $ f x
    }

instance Monad m => Monad (RaisePromote m) where
    return :: a -> RaisePromote m a
    return = Return

    (>>=) :: RaisePromote m a -> (a -> RaisePromote m b) -> RaisePromote m b
    Return x >>= k       = k x
    RaisePromote e >>= f = RaisePromote e
    (m :>>- k) >>= f     = m :>>- \x -> (k x) >>= f

    fail :: Exception -> RaisePromote m a
    fail = RaisePromote

instance Monad m => MonadRaise (RaisePromote m) where
    raise :: Exception -> RaisePromote m a
    raise = RaisePromote

instance Transformer RaisePromote where
    promote :: Monad m => m a -> RaisePromote m a
    promote m = m :>>- (\x -> Return x)  

    observe :: Monad m => RaisePromote m a -> m a
    observe (Return a)  = return a
    observe (RaisePromote e)   = fail e
    observe (m :>>- f) = m >>= \x -> observe (f x)

instance (Show (m a), Monad m) => Show (RaisePromote m a) where
    show = show . observe   
