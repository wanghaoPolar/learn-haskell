module Data where
  data Position = Cartesian Double Double | Polar Double Double

  distance :: Position -> Position -> Double
  distance (Cartesian x1 y1) (Cartesian x2 y2) =
    x1 + y1 + x2 + y2
  distance (Cartesian x1 y1) (Polar x2 y2) =
    x1 * y1 + x2 + y2

  -- 记录语法
  data Positions = MakePosition { positionX :: Double, positionY :: Double }
