module Note where

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- 匿名函数参数用空格
-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

{-
  (>100) is an operator section:
  if ? is an operator,
  then (?y) is equivalent to the function \x -> x ? y,
  and (y?) is equivalent to \x -> y ? x.
-}

combine :: (b -> c) -> (a -> b) -> (a -> c)
combine f g = f . g

-- wholemeal
foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0
