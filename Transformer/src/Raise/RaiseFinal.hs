{-# LANGUAGE PolyKinds #-} 
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Raise.RaiseFinal where

import Control.Monad
import Common.Transformer  
import Common.MonadRaise

newtype RaiseFinal (m :: * -> *) a = RaiseFinal { runRaiseFinal :: m a }

instance Functor m => Functor (RaiseFinal m) where
    fmap :: (a -> b) -> RaiseFinal m a -> RaiseFinal m b
    fmap f m = RaiseFinal $ f <$> runRaiseFinal m

instance Applicative m => Applicative (RaiseFinal m) where
    pure :: a -> RaiseFinal m a
    pure = RaiseFinal . pure

    (<*>) :: RaiseFinal m (a -> b) -> RaiseFinal m a -> RaiseFinal m b
    f <*> m = RaiseFinal $ runRaiseFinal f <*> runRaiseFinal m 

instance Monad m => Monad (RaiseFinal m) where
    return :: a -> RaiseFinal m a
    return = RaiseFinal . return

    (>>=) :: RaiseFinal m a -> (a -> RaiseFinal m b) -> RaiseFinal m b
    m >>= f = RaiseFinal $ runRaiseFinal m >>= \x -> runRaiseFinal $ f x 

    fail :: Exception -> RaiseFinal m a
    fail = RaiseFinal . fail

instance Monad m => MonadRaise (RaiseFinal m) where
    raise :: Exception -> RaiseFinal m a
    raise = fail

instance Transformer RaiseFinal where
    promote :: Monad m => m a -> RaiseFinal m a
    promote = RaiseFinal

    observe :: Monad m => RaiseFinal m a -> m a
    observe = runRaiseFinal

instance (Show (m a), Monad m) => Show (RaiseFinal m a) where
    show = show . observe   
