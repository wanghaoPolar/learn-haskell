{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}

module FingerTree where

import Data.Monoid

data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)

tag :: Tree v a -> v
tag (Leaf v _)     = v
tag (Branch v _ _) = v

instance Measured a v => Measured (Tree v a) v where
    measure = tag

class Monoid v => Measured a v where
    measure :: a -> v

newtype Size = Size Int
  deriving (Ord, Eq, Show, Num)

instance Monoid Size where
    mempty  = 0
    mappend = (+)

instance Measured a Size where
    measure _ = 1

newtype Priority = Priority Int
  deriving (Ord, Eq, Show, Num)

instance Monoid Priority where
    mempty  = Priority (maxBound::Int)
    mappend = min

branch :: Monoid v => Tree v a -> Tree v a -> Tree v a
branch x y = Branch (tag x <> tag y) x y

leaf :: Measured a v => a -> Tree v a
leaf a = Leaf (measure a) a

search :: Measured a v => (v -> Bool) -> Tree v a -> Maybe a
search p t
    | p (measure t) = Just (go mempty p t)
    | otherwise     = Nothing
    where
    -- 找到 a1 <> a2 <> a3 ... <> ak 从 False 变成 True 的点
    go i p (Leaf _ a) = a
    go i p (Branch _ l r)
        | p (i <> measure l) = go i p l
        | otherwise          = go (i <> measure l) p r

-- t !! k   = search (> k)
-- winner t = search (== measure t)
