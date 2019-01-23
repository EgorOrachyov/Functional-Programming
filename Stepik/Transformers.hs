module Transformers where

    import Control.Monad.Trans.Writer
    import Control.Monad.Trans.Reader
    import Control.Monad.Trans.Class
    import Control.Monad.Trans.State
    import Data.Char
    import Data.Monoid


    logFirstAndRetSecond :: ReaderT [String] (Writer String) String
    logFirstAndRetSecond = do
        e1 <- asks head
        e2 <- asks (\x -> map toUpper (head (tail x)))
        lift $ tell e1
        return e2

    list = ["abc","defg","klmng"]

    res = runWriter (runReaderT logFirstAndRetSecond list)


    separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
    separate p1 p2 [] = return []
    separate p1 p2 (x:xs) = do
        if (p1 x) 
        then if (p2 x)
             then ( do { tell [x]; lift $ (tell [x]); separate p1 p2 xs; } )
             else ( do { tell [x]; separate p1 p2 xs; } )
        else if (p2 x) 
             then ( do { lift $ (tell [x]); separate p1 p2 xs; } )
             else ( do { ss <- separate p1 p2 xs; return (x:ss); } )

    sep = (runWriter . runWriterT) $ separate (<3) (>7) [0..10]

    {- ====================================================================================== -}

    newtype StrictWriter w a = StrictWriter { runStrictWriter :: (a, w) }

    instance Functor (StrictWriter w) where
      fmap f  = StrictWriter . updater . runStrictWriter
        where updater (x, log) = (f x, log)

    instance Monoid w => Applicative (StrictWriter w) where
      pure x  = StrictWriter (x, mempty)
      
      f <*> v = StrictWriter $ updater (runStrictWriter f) (runStrictWriter v)
        where updater (g, w) (x, w') = (g x, w `mappend` w')

    actionStrict = StrictWriter (42,"Hello!")    

    t1 = fst . runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)
    t2 = runStrictWriter $ take 5 <$> sequenceA (repeat actionStrict)

    {- ====================================================================================== -}

    data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show)

    numberAndCount :: Tree () -> (Tree Integer, Integer)
    numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)

    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go t = do
        case t of
            Leaf _     -> do { s <- get; put (s + 1); lift $ tell (Sum 1); return (Leaf s); }
            Fork l _ r -> do { l' <- go l; s <- get; put (s + 1); r' <- go r; return (Fork l' s r'); }


    treeTest = numberAndCount (Fork (Leaf ()) () (Leaf ()))    

    {- ====================================================================================== -}
 
