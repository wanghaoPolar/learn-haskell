## how to view List
You can view lists as non-deterministic computations. A value like 100 or "what" can be viewed as a deterministic computation that has only one result, whereas a list like [1,2,3] can be viewed as a computation that can't decide on which result it wants to have, so it presents us with all of the possible results.

So when you do something like (+) <$> [1,2,3] <\*> [4,5,6], you can think of it as adding together two non-deterministic computations with +, only to produce another non-deterministic computation that's even less sure about its result.

## how to view IO as an Applicative
```haskell
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)  
```
If <\*> were specialized for IO it would have a type of
` (<\*>) :: IO (a -> b) -> IO a -> IO b. `

```haskell
myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b  

myAction :: IO String  
myAction = (++) <$> getLine <*> getLine  

main = do  
    a <- (++) <$> getLine <*> getLine  
    -- a <- myAction
    putStrLn $ "The two lines concatenated turn out to be: " ++ a  
```

## -> a as an Applicative
```haskell
instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)  

(+) <$> (+3) <*> (*100) :: (Num a) => a -> a
pure (+) <*> (+3) <*> (*100)

pure (+) = \_ -> (+)
f x (g x)
\x -> (+) (g x)
```

## zipList
```haskell
instance Applicative ZipList where  
        pure x = ZipList (repeat x)  
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)  
```
