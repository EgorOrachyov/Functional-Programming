{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Raise.RaiseE where

import Control.Monad
import Common.MonadRaise
import Common.Transformer

newtype RaiseE m a = RaiseE { runRaiseE :: Either Exception (m a) }

instance Functor m => Functor (RaiseE m) where
    fmap f m = RaiseE $ fmap (fmap f) (runRaiseE m)

instance Applicative m => Applicative (RaiseE m) where
    pure = RaiseE . pure . pure
    fs <*> xs = RaiseE $ do 
        mf <- runRaiseE fs
        mx <- runRaiseE xs
        return (mf <*> mx)

instance Monad m => Monad (RaiseE m) where
    return = pure

    m >>= k = RaiseE $ do
        ma <- runRaiseE m
        return $ do 
            a <- ma
            either (\e -> fail e) (id) (runRaiseE $ k a)

    fail = RaiseE . fail       

instance Monad m => MonadRaise (RaiseE m) where
    raise = fail

instance Transformer RaiseE where
    promote = RaiseE . pure 
    observe m = either (\e -> fail e) (id) (runRaiseE m) 