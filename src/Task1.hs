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

-- | Helper function to calculate `func` in each neighbour points of the polygon
helper  :: (Num a) => (Point -> Point -> a) -> Point -> [Point] -> a
helper _ _ [] = 0
helper func first [x] = func x first
helper func first (x:xs) = func x (head xs) + helper func first xs 

-- | Calculate perimeter of the polygon 
perimeter  :: [Point] -> Double
perimeter [] = 0
perimeter list@(first:_) = helper distance first list

-- | Calculate double area of the polygon 
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea list@(first:_) = helper crossProduct first list