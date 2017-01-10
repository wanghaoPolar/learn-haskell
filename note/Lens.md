# 透镜组
```Haskell
-- a 是需要操作的数据类型
-- 把是数据本身的类型
type Lens b a = Functor f => (a -> f a) -> b -> f b

positionXLens :: Functor f => (Double -> f Double) -> Position -> f Position
positionXLens f p = fmap (\x' -> setPositionX x' p) $ f (positionX p)
  where
    setPositionX :: Double -> Position -> Position
    setPositionX x' p = p { positionX = x' }

newPosition :: Position
newPosition = positionXLens (\x -> Maybe (x + 1)) (Position 3 4)
-- Just (Position { positionX = 4.0, positionY = 4.0 })

newPosition1 :: Position
newPosition1 = positionXLens (\x -> [x+1, x+2, x+3]) (Position 3 4)
-- [Position {4.0, 4.0}, Position {5.0, 4.0}, Position {6.0, 4.0}]
```

## getter, setter
```haskell
view :: Lens Position Double -> Position -> Double
set :: Lens Position Double -> Double -> Position -> Position
over :: Lens Position Double -> (Double -> Double) -> Position -> Position

positionXLens :: Functor f => (Double -> f Double) -> Position -> f Position
positionXLens f p = fmap setter $ f $ getter p
  where
    setter :: Double -> Position
    setter x' = p { positionX = x'}

    getter :: Position -> Double
    getter = positionX
```

## over
```haskell
over :: Lens b a -> (a -> a) -> b -> b
-- 展开后
over :: Functor f -> ((a -> f a) -> b -> f b) -> (a -> a) -> b -> b
over lens f x = runIdentity $ lifted x
  where
    lifted = lens (Identity . f)

-- 消除 lifted
over lens f x = runIdentity $ lens (Identity . f) x
-- point free
over lens f = runIdentity $ lens (Identity . f)
```

## setter
```haskell
set :: ((a -> Identity a) -> b -> Identity b) -> a -> b -> b
set lens a' x = over lens (\_ -> a') x
-- 使用 const:
-- const :: a -> b -> a
-- const x _ = x
set lens a' = over lens (const a)
```

模拟一次计算过程
```haskell
set xLens 3 (Position 1 2)

over xLens (const 3) (Position 1 2)

runIdentity $ xLens (Identity . (const 3)) (Position 1 2)

runIdentity (
  fmap
    (\x' -> (Position 1 2) { positionX = x'})
    ((Identity . (const 3)) (positionX (Position 1 2)))
)

runIdentity (
  fmap
    (\x' -> (Position 1 2) { positionX = x'})
    ((Identity . (const 3)) 1)
)

runIdentity (
  fmap
    (\x' -> (Position 1 2) { positionX = x'})
    (Identity 3)
)

runIdentity (
  Identity ( (Position 1 2) {positionX = 3})
)

runIdentity (Identity (Position 3 2))

Position 3 2
```

## view
```haskell
view :: ((a -> Const a a) -> b -> Const a b) -> b -> a
view lens = getConst . (lens Const)
```

模拟一次计算过程
```haskell
view xLens (Position 1 2)

getConst ((xLens Const) (Position 1 2))

getConst (
  fmap (\x' -> (Position 1 2) { positionX = x'})
    (Const (positionX (Position 1 2)))
)

getConst (
  fmap (\x' -> (Position 1 2) { positionX = x'})
    (Const 1)
)

getConst (Const 1)

1
```

## 透镜组可组合
```haskell
set (endLens . yLens) 5 line1
```
