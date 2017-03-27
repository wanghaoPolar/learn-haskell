# 函子

## 函子是一类物体的概括：
- 列表：由多个元素的物体
- Maybe： 有可能不存在的物体

> 函子可以把范畴 C 态射成另外一个范畴 D。

## fmap：包裹在盒子里的值，处理完成后再放回盒子里
```haskell
fmap :: (a -> b) -> f a -> f b
-- 需要满足的要求：
fmap id === id
fmap (f . g) = fmap f . fmap g
```

fmap 可以把一个 a -> b 类型的函数编程 f a -> f b 类型的函数，从而让函数
可以作用在任意类型的包裹上，这个过程叫做升格（lift）。

## 函子类型类就是实现了 fmap 的类
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap _ Nothing = Nothing

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

-- 函数本身也是一个函子，改变函数的值就是通过组合函数
-- 函数 a -> b 可以看作是包在 (->) a 函子里的 b 类型的值
-- 用 b -> c 的函数修改这个函数，就变成了一个包在 (->) a 函子里的 c 类型的值
instance Functor ((->) a) where
  fmap = (.)
```

### 两个有趣的函子
```haskell

newtype Identity a = Identity { runIdentity :: a}

Identity 3 :: Identity Int

runIdentity $ Identity 3
-- 3
instance Functor Identity where
  fmap f = Identity . f . runIdentity

-- 幻影类型
newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
  fmap f c = c

```
