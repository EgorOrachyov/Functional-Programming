module Common.MonadRaise where

--
-- Own exception type upon String
-- May be somting else ??
--
type Exception = String

--
-- Extend monad interface with function raise to
-- throw some exception in our Monad m
--
-- Looks like MonadFail with function fail
--
class (Monad m) => MonadRaise m where
    raise :: Exception -> m a

{-
    
    -- laws --

    1) raise e >>= k == raise e

-}

