module Baby where

doubleMe x = x + x

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

conanOBrien = "It's a-me, Conan O'Brien!"

listCombined = [1,2,3,4] ++ [9,10,11,12]
stringListCombined = "hello" ++ " " ++ "world"
getByIndex = [9.4,33.2,96.2,11.2,23.25] !! 1
-- don't use float number in range --

listComprehension = [x*2 | x <- [1..10]]
listComprehension' = [x*2 | x <- [1..10], x*2 >= 12]
listComprehension'' = [ x | x <- [50..100], x `mod` 7 == 3]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

doubleComprehension :: [[Int]] -> [[Int]]
doubleComprehension xxs =
  [ [ x | x <- xs, even x ] | xs <- xxs]

let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
