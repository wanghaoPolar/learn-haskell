module List where
  {-
    定义边界条件（什么时候递归终止）
    定义递归规则

    !! 在函数递归的过程中，数据没有复制，仅仅是新建了一个列表，以及一个指向后半段的指针
  -}
  lengthOfList :: [a] -> Int
  lengthOfList [] = 0
  lengthOfList (_:xs) = 1 + lengthOfList xs

  initPart :: [a] -> [a]
  initPart [] = error "no init part"
  initPart [x] = []
  initPart (x:xs) = x : initPart xs

  lastOne :: [a] -> a
  lastOne [] = error "no last one"
  lastOne [x] = x
  lastOne (_:xs) = lastOne xs

  takeN :: Int -> [a] -> [a]
  takeN 0 _ = []
  takeN _ [] = []
  takeN n (x:xs) = x : takeN (n-1) xs

  discardN :: Int -> [a] -> [a]
  discardN 0 xs = xs
  discardN _ [] = []
  discardN n (_ : xs) = discardN (n - 1) xs

  replicateList :: Int -> a -> [a]
  replicateList 0 _ = []
  replicateList n x = x : replicateList (n - 1) x

  zipList :: [a] -> [b] -> [(a,b)]
  zipList [] _ = []
  zipList _ [] = []
  zipList (x:xs) (y:ys) = (x,y) : zipList xs ys

  zipWithFunction :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWithFunction _ [] _ = []
  zipWithFunction _ _ [] = []
  zipWithFunction f (x:xs) (y:ys) = f x y : zipWithFunction f xs ys

  lMap :: (a -> b) -> [a] -> [b]
  lMap _ [] = []
  lMap f (x:xs) = f x : lMap f xs

  lFilter :: (a -> Bool) -> [a] -> [a]
  lFilter _ [] = []
  lFilter f (x:xs) =
    if f x then x : lFilter f xs
      else lFilter f xs

  lfoldl :: (b -> a -> b) -> b -> [a] -> b
  lfoldl _ acc [] = acc
  lfoldl f acc (x:xs) = lfoldl f (f acc x) xs

  lfoldr :: (a -> b -> b) -> b -> [a] -> b
  lfoldr _ acc [] = acc
  lfoldr f acc (x:xs) = f x (lfoldr f acc xs)

  mapByFoldr :: (a -> b) -> [a] -> [b]
  mapByFoldr f = lfoldr ((:) . f) []
