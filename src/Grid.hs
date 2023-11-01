{-# LANGUAGE InstanceSigs #-}
module Grid where

import Graphics.Gloss

--datatypes for the grid
data Tile = Empty { coordinate :: Coordinate } | Wall {coordinate :: Coordinate} | Circle {coordinate :: Coordinate}
data Coordinate = MkCoordinate { xCoord :: Int, yCoord :: Int }
data Direction = Left | Right | Up | Down
data Position = MkPosition {
  x :: Float
  , y :: Float
}
type Grid = [Tile]

data Vector2 = MkVector2 {vecX :: Float, vecY :: Float}

instance Num Vector2 where
  (*) :: Vector2 -> Vector2 -> Vector2
  (MkVector2 x y) * (MkVector2 x2 y2) = MkVector2 (x * x2) (y * y2)
  (MkVector2 x y) - (MkVector2 x2 y2) = MkVector2 (x - x2) (y - y2)
  (MkVector2 x y) + (MkVector2 x2 y2) = MkVector2 (x + x2) (y + y2)
  abs (MkVector2 x y)  = MkVector2 (abs x)  (abs y)
  signum (MkVector2 x y) = MkVector2 (signum x) (signum y)
  fromInteger a = MkVector2 (fromInteger a) (fromInteger a)
  negate (MkVector2 x y) = MkVector2 (-x) (-y)

instance Fractional Vector2 where
  (MkVector2 x y) / (MkVector2 x2 y2) = MkVector2 (x / x2) (y / y2)
  fromRational = undefined
  recip = undefined


--instances for these datatypes
instance Eq Tile where
  Wall coordinate1 == Empty coordinate2 = xCoord coordinate1 == xCoord coordinate2 && yCoord coordinate1 == yCoord coordinate2
  Grid.Circle coordinate1 == Empty coordinate2 = xCoord coordinate1 == xCoord coordinate2 && yCoord coordinate1 == yCoord coordinate2
  Empty coordinate1 == Wall coordinate2 = xCoord coordinate1 == xCoord coordinate2 && yCoord coordinate1 == yCoord coordinate2
  Empty coordinate1 == Grid.Circle coordinate2 = xCoord coordinate1 == xCoord coordinate2 && yCoord coordinate1 == yCoord coordinate2
  Empty coordinate1 == Empty coordinate2 = xCoord coordinate1 == xCoord coordinate2 && yCoord coordinate1 == yCoord coordinate2
  Grid.Circle coordinate1 == Grid.Circle coordinate2 = xCoord coordinate1 == xCoord coordinate2 && yCoord coordinate1 == yCoord coordinate2
  Wall coordinate1 == Wall coordinate2 = xCoord coordinate1 == xCoord coordinate2 && yCoord coordinate1 == yCoord coordinate2
  Wall coordinate1 == Grid.Circle coordinate2 = xCoord coordinate1 == xCoord coordinate2 && yCoord coordinate1 == yCoord coordinate2
  Grid.Circle coordinate1 == Wall coordinate2 = xCoord coordinate1 == xCoord coordinate2 && yCoord coordinate1 == yCoord coordinate2

instance Show Tile where
  show (Wall _) = "Wall"
  show (Grid.Circle _) = "Circle"
  show (Empty _) = "Empty"

instance Eq Coordinate where
  (MkCoordinate x y) == (MkCoordinate x2 y2) = x == x2 && y == y2
  (MkCoordinate x y) /= (MkCoordinate x2 y2) = x /= x2 || y /= y2

tiletoPath :: Tile -> Path --convert a tile to the positions of the tile
tiletoPath (Wall coordinate) = let
                            x = xCoord coordinate
                            y = yCoord coordinate
                            p1 = ((-30 + 30 * fromIntegral x) - 420, (15 - 30 * fromIntegral y) + 480)
                            p2 = ((-30 + 30 * fromIntegral x) - 420, (-15 - 30 * fromIntegral y) + 480)
                            p3 = (30 * fromIntegral x - 420, (-15 - 30 * fromIntegral y) + 480)
                            p4 = (30 * fromIntegral x - 420, (15 - 30 * fromIntegral y) + 480)
                        in
                            [p1, p2, p3, p4]
tiletoPath (Grid.Circle coordinate) = [((-15 + 30 * fromIntegral (xCoord coordinate)) - 420, (-30) * fromIntegral (yCoord coordinate) + 480)]

realPosition :: Tile -> Position --convert tile coordinate to the pixel positions
realPosition (Grid.Circle coordinate) = MkPosition (fromIntegral (xCoord coordinate) * 30 - 30) (fromIntegral (yCoord coordinate) * 30 - 30)
realPosition (Empty coordinate) = MkPosition (fromIntegral (xCoord coordinate) * 30 - 30) (fromIntegral (yCoord coordinate) * 30 - 30)
realPosition (Wall coordinate) = MkPosition (fromIntegral (xCoord coordinate) * 30 - 30) (fromIntegral (yCoord coordinate) * 30 - 30)

positionToCoord :: Position -> Coordinate--convert position to grid coordinate
positionToCoord position = MkCoordinate {xCoord = round ((x position + 30) / 30), yCoord = round ((y position + 30) / 30)}

coordToGridIndex :: Coordinate -> Int --convert grid coordinate to index in the grid list
coordToGridIndex c = (yCoord c - 1) * 28 + xCoord c - 1

leftBlock :: Grid -> Coordinate -> Tile --find the left tile of a coordinate in a grid
leftBlock grid c = grid!!(coordToGridIndex c - 1)

rightBlock :: Grid -> Coordinate -> Tile--find the right tile of a coordinate in a grid
rightBlock grid c = grid!!(coordToGridIndex c + 1)

topBlock :: Grid -> Coordinate -> Tile--find the tile above a coordinate in a grid
topBlock grid c = grid!!(coordToGridIndex c - 28)

bottomBlock :: Grid -> Coordinate -> Tile--find the tile under a coordinate in a grid
bottomBlock grid c = grid!!(coordToGridIndex c + 28)

nextBlock :: Grid -> Direction -> Coordinate -> Tile--find the block a certain direction for a coordinate
nextBlock grid Up = topBlock grid
nextBlock grid Grid.Left = leftBlock grid
nextBlock grid Grid.Right = rightBlock grid
nextBlock grid Grid.Down = bottomBlock grid

roundTo30 :: Float -> Float --round to block if direction switched
roundTo30 value = fromInteger (round (value / 30) * 30)

validateCoordinate :: Coordinate -> Coordinate
validateCoordinate c@(MkCoordinate x y) | x < 1 = validateCoordinate c{xCoord = 1}
                                        | y < 1 = validateCoordinate c{yCoord = 1}
                                        | x > 28 = validateCoordinate c{xCoord = 28}
                                        | y > 31 = validateCoordinate c{yCoord = 31}
                                        | otherwise = c
