module Note where

-- Polymorphism
data List t = E | C t (List t)

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)

{-
  partial functions
  head, tail, init, last, and (!!) are forbidden in this course
-}

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
