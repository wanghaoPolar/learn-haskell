module Main where

type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

maybeEmployee :: (Name -> String -> Employee) ->
  (Maybe Name -> Maybe String -> Maybe Employee)
maybeEmployee f name phone = pure f <*> name <*> phone

listEmployee :: (Name -> String -> Employee) ->
  ([Name] -> [String] -> [Employee])
listEmployee = zipWith

functionEmployee :: (Name -> String -> Employee) ->
  (e -> Name) -> (e -> String) -> (e -> Employee)
-- functionEmployee f toName toPhone param = f (toName param) (toPhone param)
functionEmployee f toName toPhone = pure f <*> toName <*> toPhone
-- functionEmployee f toName toPhone = f <$> toName <*> toPhone

-- we need something like (a -> b -> c) -> (f a -> f b -> f c)
-- for functor, we have fmap
-- fmap :: (a -> b) -> f a -> f b
-- can we make a fmap2 where
-- fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
-- no
-- all that we can derive from fmap and (a -> b -> c) are:
-- h  :: a -> b -> c
-- h  :: a -> (b -> c)
-- fa :: f a
-- fb :: f b
-- fmap h :: f a -> f (b -> c)
-- fmap h fa :: f (b -> c)
-- what we need is
-- f (b -> c) -> f b -> f c

-- class Functor f => Applicative f where
--  pure  :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b

-- pure (fmap0?)  :: a             -> f a
-- fmap (fmap1?)  :: (a -> b)      -> f a -> f b
-- <*>  (fmap2?)  :: (a -> b -> c) -> f a -> f b -> f c

-- shortcut for fmap
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<$>) = fmap

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 h fa fb = (h `fmap` fa) <*> fb
-- liftA2 h fa fb = h <$> fa <*> fb

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 h fa fb fc = ((h <$> fa) <*> fb) <*> fc

main :: IO ()
main = putStrLn "Hello World"
