module Typeclassopedia where

import qualified Prelude as P

const :: a -> b -> a
const a _ = a

($) :: (a -> b) -> a -> b
f $ a = f a

id :: a -> a
id a = a

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = pure f <*> a <*> b

class Functor f where
  -- fmap id = id  (1)
  -- fmap (g . h) = (fmap g ). (fmap h) (2)
  fmap :: (a -> b) -> f a -> f b
  -- if (1) then (2)

  (<$>) :: (a -> b) -> f a -> f b
  (<$>) = fmap

  (<$) :: a -> f b -> f a
  (<$) a = fmap (const a)

  ($>) :: f a -> b -> f b
  ($>) a b = fmap (const b) a

  void :: f a -> f ()
  void x = x $> ()

data Either' a b = Left a | Right b

instance Functor [] where
  fmap _ [] = []
  fmap f (x:xs) = f x:fmap f xs

instance Functor (Either' a) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right P.$ f b

instance Functor ((->) e) where
  fmap g f = g P.. f

instance Functor ((,) e) where
  fmap f (e, a) = (e, f a)

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)


data ITree a = Leaf (P.Int -> a)
             | Node [ITree a]

instance Functor ITree where
  fmap f (Leaf g) = Leaf (fmap f g)
  fmap f (Node xs) = Node (fmap (fmap f) xs)

newtype NotAFunctor a = NotAFunctor (a -> P.Int)

newtype Compose f g x = Compose (f (g x))

-- The composition of two Functors is also a Functor.
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

data Stream a = S a (Stream a)
-- bogus Functor fmap impletention
-- satisfy (2) but not (1)
-- TODO
instance Functor Stream where
  fmap f (S a as) = S (f a) (fmap f as)

class Functor f => Applicative f where
  pure :: a -> f a
  infixl 4 <*>, *>, <*
  (<*>) :: f (a -> b) -> f a -> f b

  (*>) :: f a -> f b -> f b
  a1 *> a2 = (id <$ a1) <*> a2

  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const
