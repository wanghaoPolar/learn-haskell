module Golf where

import Data.List
import Data.Ord

skips :: [a] -> [[a]]
skips xs =
    map (map fst . jump) indexedXs
  where
    indexedXs = zip xs [1..]
    jump (_, index) =
      filter
        (\(_, i) ->
          i `mod` index == 0)
        indexedXs

localMaxima :: [Integer] -> [Integer]
localMaxima xs =
  map (\[_, y, _] -> y) $ filter
    check
    $ map (take 3) (tails xs)
  where
    checkLength l = length l == 3
    isLocalMaximum [x,y,z] = maximum [x,y,z] == y
    check items = checkLength items && isLocalMaximum items

countIntegers :: [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
countIntegers [] result = result
countIntegers (n:ns) result = countIntegers ns newResult
  where
    newResult = map (\(index, count) ->
      if index == n
        then (index, count + 1)
        else (index, count)
      )
      result

histogramBottom :: String
histogramBottom = "\n" ++ replicate 9 '=' ++ "\n" ++
  concatMap show [1..9] ++ "\n"

histogram :: [Integer] -> String
histogram xs = dataGram ++ histogramBottom
  where
    countResult = countIntegers xs (zip [1..9] $ repeat 0)
    -- comparing 函数：（b -> a) -> b -> b -> Ordering
    maxA = snd $ maximumBy (comparing snd) countResult
    -- intercalate 在 [[**00], [0*00]] 各元素之间穿插 "\n"
    dataGram = intercalate "\n" $
      map
        (map (\x -> if x == 1 then '*' else ' '))
        -- transpose 旋转矩阵
      $ transpose $
      map
        (\(_, count) ->
          genericReplicate (maxA - count) 0 ++
          genericReplicate count 1)
      countResult
