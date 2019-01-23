{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Raise.RaiseCC where

import Data.Kind
import Control.Monad
import Common.MonadRaise
import Common.Transformer

newtype RaiseCC (m :: * -> *) a = 
    RaiseCC { runRaiseCC :: forall r. (a -> m r) -> m r }

instance Functor m => Functor (RaiseCC m) where
    fmap :: (a -> b) -> RaiseCC m a -> RaiseCC m b
    fmap f m = RaiseCC $ \k -> (runRaiseCC m) (k . f)   

instance Applicative m => Applicative (RaiseCC m) where
    pure :: a -> RaiseCC m a
    pure a = RaiseCC $ \f -> f a

    (<*>) :: RaiseCC m (a -> b) -> RaiseCC m a -> RaiseCC m b
    fs <*> xs = RaiseCC $ \k -> (runRaiseCC fs) $ \f -> (runRaiseCC xs) (k . f)

instance Monad m => Monad (RaiseCC m) where
    return :: a -> RaiseCC m a
    return = pure

    (>>=) :: RaiseCC m a -> (a -> RaiseCC m b) -> RaiseCC m b
    m >>= k = RaiseCC $ \s -> (runRaiseCC m) (\a -> runRaiseCC (k a) s)

    fail :: Exception -> RaiseCC m a
    fail e = RaiseCC $ \_ -> fail e

instance Monad m => MonadRaise (RaiseCC m) where
    raise :: Exception -> RaiseCC m a
    raise e = RaiseCC $ \s -> fail e

instance Transformer RaiseCC where
    promote :: (Monad m) => m a -> RaiseCC m a
    promote m = RaiseCC $ \s -> (m >>= s)
 
    observe :: Monad m => RaiseCC m a -> m a
    observe m = runRaiseCC m $ return

instance (Show (m a), Monad m) => Show (RaiseCC m a) where
    show = show . observe 
