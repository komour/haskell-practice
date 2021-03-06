{-# LANGUAGE BangPatterns #-}

module Task1
  ( Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  ) where

import           Control.DeepSeq (NFData, rnf)

data Point = Point !Int !Int
    deriving (Show)

instance NFData Point where
  rnf (Point x y) = rnf x `seq` rnf y

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) $ y1 + y2

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) $ y1 - y2

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | Calculate distance between to points
distance :: Point -> Point -> Double
distance a b = sqrt $ fromIntegral $ scalarProduct sub sub
  where
    sub = b `minus` a

-- | Helper-function to calculate given function in each two neighbour points of the polygon
calculate :: (Num a) => a -> (Point -> Point -> a) -> Point -> [Point] -> a
calculate acc _ _ []            = acc
calculate !acc func first [x]    = calculate (acc + func x first) func first []
calculate !acc func first (x:xs) = calculate (acc + func x (head xs)) func first xs

-- | Calculate perimeter of the polygon
perimeter :: [Point] -> Double
perimeter []             = 0
perimeter list@(first:_) = calculate 0 distance first list

-- | Calculate double area of the polygon
doubleArea :: [Point] -> Int
doubleArea []             = 0
doubleArea list@(first:_) = calculate 0 crossProduct first list