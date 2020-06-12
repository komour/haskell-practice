module Task1
  ( Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  ) where

data Point = Point Int Int
    deriving (Show)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) $ y1 + y2

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) $ y1 - y2

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | Calculate distance between to points
distance :: Point ->  Point -> Double
distance a b = sqrt $ fromIntegral $ scalarProduct sub sub
  where sub = b `minus` a

-- | Helper function with memorization of the first element to calculate perimeter of the polygon
perimHelper  :: Point -> [Point] -> Double
perimHelper _ [] = 0
perimHelper first [x] = distance x first
perimHelper first (x:xs) = distance x (head xs) + perimHelper first xs 

-- | Calculate perimeter of the polygon 
perimeter  :: [Point] -> Double
perimeter [] = 0
perimeter list@(first:_) = perimHelper first list

-- | Calculate double area of the polygon 
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea list@(first:_) = areaHelper first list

-- | Helper function with memorization of the first element to calculate double area of the polygon
areaHelper  :: Point -> [Point] -> Int
areaHelper _ [] = 0
areaHelper first [x] = crossProduct x first
areaHelper first (x:xs) = crossProduct x (head xs) + areaHelper first xs 