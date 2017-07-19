{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module StateT where

import Core
import Id
import Optional
import List
import Functor
import Applicative
import Monad
import State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
firstST :: (a -> b) -> (a, s) -> (b, s)
firstST f (a, s) = (f a, s)

instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  a2b <$> s2fas = StateT $ \s ->
    let fas = runStateT s2fas s
    in  firstST a2b <$> fas

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure a = StateT $ \s -> pure (a, s)
  (<*>) ::
   StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  s2fa2b <*> s2fa = StateT $ \s ->
    runStateT s2fa2b s >>= (\(a2b, s') -> let fa = runStateT s2fa s' in firstST a2b <$> fa)

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  a2s2fb =<< s2fa = StateT $ \s ->
    let fa = runStateT s2fa s
    in  fa >>= (\(a, s') -> let s2fb = a2s2fb a in runStateT s2fb s')

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' s2a = StateT $ \s -> Id (s2a s)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' s'sa s = let (Id as) = runStateT s'sa s in as

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT stfa s = snd <$> runStateT stfa s

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' stfa s = let (Id (_, s')) = runStateT stfa s in s'

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT stfa s = fst <$> runStateT stfa s 

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' stfa s = let (Id (a, _)) = runStateT stfa s in a 

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Monad f =>
  StateT s f s
getT = StateT $ \s -> pure(s, s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT s = StateT $ const $ pure ((), s)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
checker :: (Ord a, Num a) => a -> State' (S.Set a) Bool
checker a = state' (\s ->
  if S.member a s then (False, s) else (True, S.insert a s))

distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' as = eval' (filtering checker as) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
checkerF :: (Ord a, Num a) => a -> StateT (S.Set a) Optional Bool
checkerF a = StateT $ \s ->
  if | a > 100 -> Empty
     | S.member a s -> Full (False, s)
     | otherwise -> Full (True, S.insert a s)

distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF as = evalT (filtering checkerF as) S.empty

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) ::
      (a -> b)
      -> OptionalT f a
      -> OptionalT f b
  f <$> OptionalT a = OptionalT ((<$>) ((<$>) f) a)

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Applicative f => Applicative (OptionalT f) where
  pure =
    OptionalT . pure . pure
  OptionalT f <*> OptionalT a =
    OptionalT (lift2 (<*>) f a)

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) :: (a -> OptionalT f b) -> OptionalT f a -> OptionalT f b
  -- f =<< (OptionalT a) = OptionalT $ join result'
  --   where
  --     result = ((<$>) f) <$> a
  --     getOp (Full re) = runOptionalT re
  --     getOp Empty = pure Empty
  --     result' = getOp <$> result
  -- =<< 就是（拿到 f 里面的值）
  f =<< OptionalT x =
    OptionalT (x >>= (\o -> case o of
                              Empty -> pure Empty
                              Full a -> runOptionalT (f a)))

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  f <$> (Logger ls a) = Logger ls $ f a

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure a = Logger Nil a
  (Logger ls1 f) <*> (Logger ls2 a) = Logger (ls1 ++ ls2) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  f =<< (Logger ls1 a) =
    let (Logger ls2 b) = f a
    in  Logger (ls1 ++ ls2) b

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l a = Logger (l:.Nil) a

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
checkerG :: (Integral a, Show a) => a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
checkerG a = StateT $ \s ->
  OptionalT (
    if a > 100
      then log1 (fromString $ "aborting > 100: " P.++ show a) Empty
      else (if even a
              then log1 (fromString $ "even number: " P.++ show a)
              else pure) $ Full (a `S.notMember` s, a `S.insert` s)
  )

distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG as = runOptionalT $ evalT (filtering checkerG as) S.empty
