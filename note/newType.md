## 类型别名 type
```haskell
type IntList = [Int]
```

## 新的类型 newType

newType 允许你定义一个只包含 *一个* 只接收 *一个* 参数的构造函数的数据类型。
```haskell
newtype Cm = Cm Double deriving Eq
