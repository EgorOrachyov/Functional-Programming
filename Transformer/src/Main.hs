{-# LANGUAGE PolyKinds, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

-- import Raise.Raise
-- import Raise.RaiseR
-- import Raise.RaiseE
-- import Raise.RaiseCC
-- import Raise.RaiseFinal
-- import Raise.RaisePromote

import Control.Monad
import Common.MonadRaise
import Common.Transformer
import Control.Monad.State


data Fix (f :: * -> *) = Fix (f (Fix f))

fix :: f (Fix f) -> Fix f
fix m = Fix m

unfix :: Fix f -> f (Fix f)
unfix (Fix m) = m

type family Base f :: * -> *
type instance Base (Fix f) = f

data RaiseF (m :: * -> *) a r = ReturnF a
                              | forall (b :: * -> *). BindF (m (b a)) ((b a) -> r)
                              | RaiseF Exception

-- b :: * -> * 
--                               

type Raise m a = Fix (RaiseF m a)

instance Functor (RaiseF m x) where
    fmap :: (a -> b) -> RaiseF m x a -> RaiseF m x b
    fmap f (BindF x r) = BindF x (\v -> f (r v) )
    fmap f (RaiseF a) = RaiseF a
    fmap f (ReturnF e) = ReturnF e

cata :: (RaiseF m a r -> r) -> Fix (RaiseF m a) -> r
cata f (Fix (ReturnF a)) = f (ReturnF a)
cata f (Fix (RaiseF e))  = f (RaiseF e)
cata f (Fix (BindF m g)) =  
cata _ _ = undefined

-- g :: (b a) -> RaiseF m a (RaiseF )
--

-- alg ReturnF a = 0
-- alg RaiseF e   = 2
-- alg BindF _ f

main :: IO ()
main = print "Hello, World! I want to believe that all works..."