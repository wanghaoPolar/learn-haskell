module Position where

data Position = MakePosition Double Double

newPosition :: Position
newPosition = MakePosition 1.5 2

{-
distance :: Position -> Position -> Double
distance p1 p2 =
case p1 of
MakePosition x1 y1 ->
case p2 of
MakePosition x2 y2 ->
sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
-}

distance :: Position -> Position -> Double
distance (MakePosition x1 y1) (MakePosition x2 y2) =
  sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

pointA :: Position
pointA = MakePosition 0 0

pointB :: Position
pointB = MakePosition 3 4
