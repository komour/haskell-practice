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
--time                 82.52 ms   (71.48 ms .. 91.74 ms)

benchPerimeterNaive :: Benchmark
benchPerimeterNaive = bench "perimeter-naive" $ nf N.perimeter generatePolygonNaive
--time                 1.151 s    (311.3 ms .. 3.052 s)

benchArea :: Benchmark
benchArea = bench "area" $ nf T.doubleArea generatePolygon
--time                 78.33 ms   (76.01 ms .. 81.66 ms)

benchAreaNaive :: Benchmark
benchAreaNaive = bench "area-naive" $ nf N.doubleArea generatePolygonNaive
--time                 596.3 ms   (313.3 ms .. 1.180 s)

perimeterBenchmark :: Benchmark
perimeterBenchmark = bgroup "Perimeter Benchmark" [benchPerimeter, benchPerimeterNaive]

areaBenchmark :: Benchmark
areaBenchmark = bgroup "Area Benchmark" [benchArea, benchAreaNaive]