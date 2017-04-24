{-# LANGUAGE MultiWayIf #-}
module JoinList where

import           Data.Monoid
import           Data.List
import           Sized
import           Scrabble

data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_  !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) =>
             Int -> JoinList b a -> Maybe a
indexJ _ Empty              = Nothing
indexJ 0 (Single _ element) = Just element
indexJ index (Append s x y) =
   if   | index >= sumSize -> Nothing
        | index >= leftSize -> indexJ (index - leftSize) y
        | otherwise        -> indexJ index x
      where
        (Size sumSize) = size s
        (Size leftSize) = size $ tag x
indexJ _ _                  = Nothing

dropJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a
dropJ 0 x = x
dropJ n (Append s x y) =
  if | n >= sumSize -> Empty
     | n >= leftSize -> dropJ (n - leftSize) y
     | otherwise -> Append (dropTag <> yTag) dropItem y
     where
      (Size sumSize) = size s
      (Size leftSize) = size $ tag x
      dropItem = dropJ n x
      dropTag = tag dropItem
      yTag = tag y
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n (Append s x y) =
  if | n >= sumSize -> Append s x y
     | n >= leftSize -> Append (takeTag <> xTag) x takeItem
     | otherwise -> takeJ n x
     where
      (Size sumSize) = size s
      (Size leftSize) = size $ tag x
      takeItem = takeJ (n - leftSize) y
      takeTag = tag takeItem
      xTag = tag x
takeJ 1 (Single b element) = Single b element
takeJ _ _ = Empty

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

fromString :: [String] -> JoinList (Score, Size) String
fromString [] = Empty
fromString [str] = Single (scoreString str, Size 1) str
fromString strList =
  Append
    (tag leftBuffer <> tag rightBuffer)
    leftBuffer
    rightBuffer
  where
    splitPoint = div (length strList) 2
    (left, right) = genericSplitAt splitPoint strList
    leftBuffer = fromString left
    rightBuffer = fromString right

replaceJ :: (Sized b, Monoid b) =>
            Int -> b -> a -> JoinList b a -> JoinList b a
replaceJ index newTag newItem (Append b left right) =
  if | index > sumSize -> (Append b left right)
     | index > leftSize -> Append (tag left <> tag newRight) left newRight
     | otherwise -> Append (tag newLeft <> tag right) newLeft right
     where
       (Size sumSize) = size b
       (Size leftSize) = size $ tag left
       newRight = replaceJ (index - leftSize) newTag newItem right
       newLeft = replaceJ index newTag newItem left
replaceJ 1 newTag newItem (Single _ _) = Single newTag newItem
replaceJ _ _ _ b = b

sample :: JoinList Size Char
sample =  Append (Size 4)
   (Append (Size 3)
     (Single (Size 1) 'y')
     (Append (Size 2)
       (Single (Size 1) 'e')
       (Single (Size 1) 'a')))
   (Single (Size 1) 'h')

sampleString :: String
sampleString = "asdfawe\nqewrkdjlasd\nqwerjksdfas\nasdlfkjwq\n"
