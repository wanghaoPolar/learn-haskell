module Moli where
  {-
    chapter 1

    普通函数的优先级最高，高于任何中缀函数
    sort xs ++ sort xs ==
    (sort xs) ++ (sort xs)

    左结合、右结合、不结合：
    infixl, infixr, infix

    :: 类型说明符优先级最低，用于说明整个式子的类型
  -}

  {-
    chapter 2 data
    data 关键字用于构造一个类型和其实例构造函数
  -}

  data Position = MakePosition Double Double
  --  也可以直接用类型名做构造函数
  --  data Position = Position Double Double
  --  用中缀函数作为构造函数，一定要以':'开头
  --  data Position = Double :+ Double

  {-
    模式匹配
  -}

  {-
    case x of
      pattern1 -> expression1
      pattern2 -> expression2
  -}

  distance :: Position -> Position -> Double
  distance p1 p2 =
    case p1 of
      MakePosition x1 y1 ->
        case p2 of
          MakePosition x2 y2 -> sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

  {-
    使用构造函数直接匹配
  -}
  distance' :: Position -> Position -> Double
  distance' (MakePosition x1 y1) (MakePosition x2 y2) =
    sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

  {-
    let
  -}
  distance'' :: Position -> Position -> Double
  distance'' p1 p2 =
    let MakePosition x1 y1 = p1
        MakePosition x2 y2 = p2
    in sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

  {-
    记录语法
    data Position = Position { getX :: Double, getY :: Double, label1 :: Type1 }
    getX positionX == let MakePosition x1 _ = PositionX in x
  -}

  {-
    chapter 3 list recursion
  -}

  {-
    chapter 4 tuple && type infer && higher order function
  -}
