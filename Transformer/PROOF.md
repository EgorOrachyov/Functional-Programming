# Prove laws

Analytically prove the fulfillment of laws for the monad Rais defined as follows:

```
data Raise (m :: * -> *) a = Return a
                           | forall b. (Raise m b) :>>= (b -> Raise m a)
                           | Raise Exception
                           | Promote (m a)
```

Monad type class instance implemented as follows:

```
instance Monad m => Monad (Raise m) where
    return :: a -> Raise m a
    return = Return

    (>>=) :: Raise m a -> (a -> Raise m b) -> Raise m b
    (>>=) = (:>>=)

    fail :: Exception -> Raise m a
    fail = Raise
```

In the following blocks Monad's laws will be prove step by step. 
We will use observe function to get into our monad and interract 
with result computations with internal monad m.

Raise Monad transformer instance defined as follows:

```
instance Transformer Raise where
    promote :: Monad m => m a -> Raise m a
    promote = Promote

    observe :: Monad m => Raise m a -> m a
    observe (Return a)  = return a
    observe (Raise e)   = fail e
    observe (Promote m) = m
    
    observe ((Return x)  :>>= k) = observe (k x)
    observe ((Raise e)   :>>= _) = fail e
    observe ((Promote m) :>>= k) = m >>= (\x -> observe (k x))
    observe ((m :>>= k)  :>>= t) = observe (m :>>= (\x -> k x :>>= t))
```

## Left unit *return x >>= k  ==  k x*                           
```
* observe $ return x >>= k == *def return*
* observe $ Return x >>= k == *def >>=*
* observe $ Return x :>>= k == *def observe*
* observe $ k x
```
## Right unit *m >>= return == m*
```
* observe $ m >>= return == *def return* 
* observe $ m >>= Return == *defined >>=*
* observe $ m :>>= Return == *multiple choices*

1. 
* observe $ Return x :>>= Return == *def observe*
* observe $ Return x

2.
* observe $ Raise e :>>= Return == *def observe*
* observe $ Raise e

3.
* observe $ Promote m :>>= Return == *def observe*
* m >>= (\x -> observe (Return x)) == *def observe*
* m >>= = (\x -> return x) == m >>= return x *m is Monad*

4.
* observe $ (m :>>= k) :>>= Return == *def observe*
* observe (m :>>= (\x -> k x :>>= Return)) == *here we have to use some induction*
```
#### For *Return x*
```
* observe $ (Return x :>>= \x -> k x :>>= Return) ==
* observe $ (\x -> k x :>>= Return) x == 
* observe $ k x :>>= Return == *and so on*
```
#### For *Promote m*
```
* observe $ (Promote m :>>= \x -> k x :>>= Return) ==
* m >>= (\x -> observe $ (\x -> k x :>>= Return) x) ==
* m >>= (\x -> observe $ k x :>>= Return) == *from previous step*
```
#### For *Raise e*
```
* observe $ (Raise e :>>= \x -> k x :>>= Return) ==
* fail e
```
#### For *m :>>= t*
```
* observe $ (m :>>= (\x -> k x :>>= Return)) == *many steps*
* m_i :>>= (\x_i -> ( ... :>>= (\x -> k x :>>= Return) ... ) x_i ) 
```
## Associativity *m >>= (\x -> k x >>= h) == (m >>= k) >>= h*
```
* observe $ (m :>>= k) :>>= h == *def observe*
* observe $ (m :>>= (\x -> k x :>>= h)) == *actually what we need*
```
#### By def translate in λ abstraction as follows in the law. Some computations:
```
1.
* observe $ (Return x :>>= (\x -> k x :>>= h) ==
* observe $ (\x -> k x :>>= h) x == 
* observe $ k x :>>= h 

2.
* observe $ (Promote m :>>= (\x -> k x :>>= h)) ==
* m >>= (\x -> observe $ (\x -> k x :>>= h) x) ==
* m >>= (\x -> observe $ k x :>>= h)

3.
* observe $ (Raise e :>>= (\x -> k x :>>= h)) ==
* fail e

4.
* ...
```
