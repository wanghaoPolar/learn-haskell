module Basic where
  curryFunction :: ((a,b) -> c) -> a -> b -> c
  curryFunction f x y = f (x,y)

  {-
    $ 函数：在左边和右边的表达式上都加上括号
    总会先计算右侧的，再计算左侧的

    & 函数：在左边和右边的表达式上都加上括号
    总会先计算左侧的，再计算右侧的
  -}

  {-
    匿名函数
    3 & (\x -> x + 1) & (\x -> x ^ 2)
  -}

  {-
    . 组合函数
    f . g = \x -> f (g x)
  -}
