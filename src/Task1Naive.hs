module Task1Naive
  ( Point(..)
  , perimeter
  , doubleArea
  ) where

import           Control.DeepSeq (NFData, rnf)

-- |Non-strict alternative for Point data type
data Point = Point Int Int
    deriving (Show)

instance NFData Point where
  rnf (Point x y) = rnf x `seq` rnf y

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) $ y1 - y2

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- |Calculate distance between to points
distance :: Point -> Point -> Double
distance a b = sqrt $ fromIntegral $ scalarProduct sub sub
  where
    sub = b `minus` a

-- |Helper-function to calculate given function in each two neighbour points of the polygon (Naive version)
calculate :: (Num a) => (Point -> Point -> a) -> Point -> [Point] -> a
calculate _ _ []            = 0
calculate func first [x]    = func x first
calculate func first (x:xs) = func x (head xs) + calculate func first xs

-- |Calculate perimeter of the polygon (Naive version)
perimeter :: [Point] -> Double
perimeter []             = 0
perimeter list@(first:_) = calculate distance first list

-- |Calculate double area of the polygon (Naive version)
doubleArea :: [Point] -> Int
doubleArea []             = 0
doubleArea list@(first:_) = calculate crossProduct first list
