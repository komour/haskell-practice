module Main where

import qualified Task1          as T (Point (..), doubleArea, perimeter)
import qualified Task1Naive     as N (Point (..), doubleArea, perimeter)

import           Criterion.Main

main :: IO ()
main = defaultMain [perimeterBenchmark, areaBenchmark]

generatePolygonNaive :: [N.Point]
generatePolygonNaive = [N.Point x 0 | x <- [0..5000000]] ++ [N.Point 5000000 y | y <- [0..5000000]]

generatePolygon :: [T.Point]
generatePolygon = [T.Point x 0 | x <- [0..5000000]] ++ [T.Point 5000000 y | y <- [0..5000000]]

benchPerimeter :: Benchmark
benchPerimeter = bench "perimiter" $ nf T.perimeter generatePolygon
--time                 988.7 ms   (307.2 ms .. 2.566 s)

benchPerimeterNaive :: Benchmark
benchPerimeterNaive = bench "perimeter-naive" $ nf N.perimeter generatePolygonNaive
--time                 607.9 ms   (317.3 ms .. 1.247 s)

benchAreaNaive :: Benchmark
benchAreaNaive = bench "area-naive" $ nf N.doubleArea generatePolygonNaive
--time                 596.3 ms   (313.3 ms .. 1.180 s)

benchArea :: Benchmark
benchArea = bench "area" $ nf T.doubleArea generatePolygon
--time                 489.3 ms   (-42.04 ms .. 864.4 ms)

perimeterBenchmark :: Benchmark
perimeterBenchmark = bgroup "Perimeter Benchmark" [benchPerimeterNaive, benchPerimeter]

areaBenchmark :: Benchmark
areaBenchmark = bgroup "Area Benchmark" [benchAreaNaive, benchArea]