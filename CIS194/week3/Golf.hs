module Golf where

import Data.List
import Data.Ord

-- TODO make it readable
skips :: [a] -> [[a]]
skips xs =
    map (fst . unzip . jump) indexedXs
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
    isMaximum [x,y,z] = maximum [x,y,z] == y
    check items = checkLength items && isMaximum items

histogram :: [Integer] -> String
histogram xs = dataGram ++ bottom
  where
    func arr num =
      map
        (\(index, count) ->
          if index == num
            then (index, count + 1)
            else (index, count)
        )
        arr
    result = foldl' func (zip [1..9] $ repeat 0) xs
    maxA = snd $ maximumBy (comparing snd) result
    bottom = "\n" ++ replicate 9 '=' ++ "\n" ++
      concatMap show [1..9] ++ "\n"
    dataGram = intercalate "\n" $
      map
        (map (\x -> if x == 1 then '*' else '0'))
      $ transpose $
      map
        (\(_, count) ->
          genericReplicate (maxA - count) 0 ++
          genericReplicate count 1)
      result
