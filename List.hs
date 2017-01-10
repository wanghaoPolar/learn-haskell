module List where
  {-
    定义边界条件（什么时候递归终止）
    定义递归规则

    !! 在函数递归的过程中，数据没有复制，仅仅是新建了一个列表，以及一个指向后半段的指针
  -}
  initPart :: [a] -> [a]
  initPart [] = error "error"
  initPart [_] = []
  initPart (x : xs) = x : initPart xs

  lastOne :: [a] -> a
  lastOne [] = error "error"
  lastOne [x] = x
  lastOne (_ : xs) = lastOne xs

  takeN :: Int -> [a] -> [a]
  takeN 0 _ = []
  takeN _ [] = []
  takeN n (x : xs) = x : takeN (n - 1) xs

  discardN :: Int -> [a] -> [a]
  discardN 0 xs = xs
  discardN _ [] = []
  discardN n (_ : xs) = discardN (n - 1) xs

  replicateList :: Int -> a -> [a]
  replicateList 0 _ = []
  replicateList n x = x : replicateList (n - 1) x
