module Moli where

foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft _ acc [] = acc
foldLeft f acc (x:xs) = foldLeft f (f acc x) xs

foldRight :: (b -> a -> a) -> a -> [b] -> a
foldRight _ acc [] = acc
foldRight f acc (x:xs) = f x (foldRight f acc xs)

-- 所有的函数都是单参数的函数
foldLeftByFoldRight :: (a -> b -> a) -> a -> [b] -> a
foldLeftByFoldRight f a bs =
  foldr (\b g x -> g (f x b)) id bs a

-- 折叠是顺序操作的抽象
mapByFoldRight :: (a -> b) -> [a] -> [b]
-- (:) . f = \x -> (:)(f x)
-- 返回一个完成了一半的函数，再加上右边的结果
-- ((:) a) :: [a] -> [a]
mapByFoldRight f = foldRight ((:) . f) []

lengthByFoldRight :: [a] -> Integer
lengthByFoldRight = foldr (\_ acc -> acc + 1) 0

maximumByFoldRight :: (Ord a) => [a] -> Maybe a
maximumByFoldRight [] = Nothing
maximumByFoldRight (x:xs) = Just (foldr max x xs)

lastByFoldRight :: [a] -> Maybe a
lastByFoldRight [] = Nothing
lastByFoldRight (x:xs) = Just (foldr (\a b -> b) x xs)

minSubList :: (Num a, Ord a) => [a] -> Int -> a
minSubList xs m = initSum + minDiff
  where
    (initXs, shifted) = splitAt m xs
    initSum = sum initXs
    minDiff = minimum $ scanl (+) 0 $ zipWith (-) shifted xs

{-
  类型类 type class
-}

data Position = Cartesian Double Double | Polar Double Double
instance Eq Position where
  Cartesian x1 y1 == Cartesian x2 y2 = (x1 == x2) && (y1 == y2)

{-
  type: 类型别名
  newtype: 只允许有一个一个参数的构造函数的 data，在底层打包过程会消失
-}

{-
  底： undefined (_|_)
  底层类型： 不包含 (_|_) 的类型，如 Double#，
    任何01排列都可能是合法值
    无法判断计算是否失败8
-}

{-
  惰性求值
-}
