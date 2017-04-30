module Main where

(*>)       :: Applicative f => f a -> f b -> f b
fa *> fb = fb

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa fb = pure f <*> fa <*> fb

seqA :: Applicative f => [f a] -> f [a]
seqA = foldr (liftA2 (:)) (pure [])

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f = seqA . fmap f

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n fa = pure (replicate n) <*> fa

repeatA :: Applicative f => f a -> f [a]
repeatA fa = pure repeat <*> fa
