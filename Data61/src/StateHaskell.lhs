State Monad

oldState -> (someValue, newState)

例如对于纸牌游戏的抽象，
state 是牌堆的状态
value 的不同操作的结果
比如
countCards cs = （length cs, cs)
sortCard cs = ((), sort cs)
popCard cs = (head cs, tail cs)
pushCard cs c = ((), c : cs)

> newtype State s a = State { runState :: s -> (a, s) }
>
> -- fmap :: (a -> b) -> State s a -> State s b 底层表示为
> -- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
> countCards :: State CardStack Int
> countCards = pure . length
> 
> sortCard :: State CardStack ()
> sortCard = State $ \s -> ((), sort s)
>
> popCard :: State CardStack Card
> popCard = State $ \s -> (head s, tail s)
>
> pushCard :: Card -> State CardStack ()
> pushCard c = State $ \s -> ((), c : s)
>
> myCardStack :: CardStack
> myCardStack = [C_8, C_J, C_2]
>
> op :: State CardStack Card
> op = do
>   sortCard
>   pushCard C_Q
>   sortCard
>   popCard
>
> runState op myCardStack
>
get 直接把当前的状态作为计算结果包裹到函子里，用于方面地取出状态
> get :: State s s
> get = State $ \s -> (s, s)
put 接受一个新的状态 s 来替换之前的状态
> put :: s -> State s ()
> put s = State $ \_ -> ((), s)
modify 修改内部状态
> modify :: (s -> s) -> State s ()
> modify f = State $ \s -> ((), f s)

> op2 :: State CardStack Card
> op2 = do
>   put [C_8, C_J, C_2]
>   modify sort
>   
>   replicateM_ 3 $ do
>     cs <- get
>     when (length cs < 10) $
>       pushCard C_2
用 get 取出再使用函数处理的组合很常见，可以写一个辅助函数
> gets :: (s -> a) -> State s a
> gets f = State $ \s -> (f s, s)

## wiki haskell

> type Seed = Int
> randomNext :: Seed -> Seed 
> randomNext rand = 
>   if newRand > 0 
>     then newRand 
>     else newRand + 2147483647 
>   where 
>     (hi,lo) = rand `divMod` 127773
>     newRand = 16807 * lo - 2836 * hi 
> rollDie :: Seed -> (Int, Seed) 
> rollDie seed = ((seed `mod` 6) + 1, randomNext seed)
> sumTwoDice :: Seed -> (Int, Seed) 
> sumTwoDice seed0 = 
>   let (die1, seed1) = rollDie seed0
>       (die2, seed2) = rollDie seed1 
>   in (die1 + die2, seed2)
