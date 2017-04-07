## week2
### 语法
- case of
```haskell
ex03 = case "Hello" of
           []      -> 3
           ('H':s) -> length s
           _       -> 7
```
- where
```haskell
case i `rem` 5 of
  0 -> x : rest
  _ -> rest
  where
    rest = go xs

case xs of
  (x:xs') -> ...
    where
      definitions
  [] -> ...
    where
      definitions
```

- guard
```haskell
amIRich money =
  | money < 0   = "You're bronken, sir"
  | otherwise   = "You're very rich, sir"

isListLong xs
  | l < 10      = "This list is not long"
  | l >= 100    = "This list is very long"
  | otherwise   = "This list is long"
```

- MultiWayIf
```haskell
ex03 =
  if | 1 < 0 -> 2
     | 2 > 1 -> 1
