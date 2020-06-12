module Main where

import           Task1          (Point (..), perimeter)

import           Criterion.Main

main :: IO ()
main = defaultMain [stupidBenchmark]

generatePolygon :: [Point]
generatePolygon = [Point x 0 | x <- [0..5000000]] ++ [Point 5000000 y | y <- [0..5000000]]

stupidBenchmark :: Benchmark
stupidBenchmark = bench "perimeter-v0" $ nf perimeter generatePolygon
-- time                 570.4 ms   (239.0 ms .. 1.270 s)
--                      0.848 R²   (0.767 R² .. 1.000 R²)
-- mean                 564.4 ms   (372.7 ms .. 701.9 ms)
-- std dev              196.8 ms   (106.9 ms .. 274.8 ms)