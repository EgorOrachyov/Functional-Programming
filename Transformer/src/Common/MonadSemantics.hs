{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Common.MonadSemantics where

import MonadRaise

--
-- Semantic for monad calculation in tagless
-- final style (an attempt to rewrite Raise
-- without constructors)
--
-- t : (* -> *) -> * -> *
--
class MonadSemantics (t :: (* -> *) -> * -> *) where
    return  :: Monad m => a -> t m a
    bind    :: Monad m => t m a -> (a -> t m b) -> t m b
    raise   :: Monad m => Exception -> t m a
    promote :: Monad m => m a -> t m a

{-

    -- laws --

    1) Monad for return and bind

    2) MonadRaise for raise

    3) Transformer for promote

-}
