module Main where

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (ma:mas) = ma >>= \a -> (:) <$> pure a <*> sequence mas
-- moli haskell 这里写错了
-- sequence (a:as) = a >>= \x -> x : sequence as

main :: IO ()
main = putStrLn "Hello World"
