{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Common.Transformer where

--
-- Custom class for Monad Transformer which alows
-- lift m a to the level of transformer t
-- and observe effects via getting value m a from 
-- transformer t
--
-- promote <=> lift for MonadTrans
--
class Transformer (t :: (* -> *) -> * -> *) where
    promote :: (Monad m) => m a -> t m a
    observe :: (Monad m) => t m a -> m a

{-

    -- laws --
    
    1) promote (return a)           == return a

    2) promote (m >>= k)            == m >>= (promote . k)

    3) observe (return a)           == return a

    4) observe (promote m >>= k)    == m >>= (observe . k) 

-}
