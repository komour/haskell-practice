module Main where

import           ConcurrentHashTable
import qualified Control.Concurrent.Thread as Th
import qualified Task1                     as T (Point (..), doubleArea,
                                                 perimeter)
import qualified Task1Naive                as N (Point (..), doubleArea,
                                                 perimeter)

import           Control.Monad             (forM_)
import           Criterion.Main
import           System.Random

main :: IO ()
main = defaultMain [perimeterBenchmark, areaBenchmark, chtBenchmark]

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

chtBenchmark :: Benchmark
chtBenchmark = bench "Hashtable: read/insert 10^5 elements via 4 threads" $ nfIO stressCht
-- time                 10.26 s    (7.048 s .. 12.93 s)

stressCht :: IO ()
stressCht = do
  ht <- newCHT
  let lists = [[0..25000::Int], [0..25000], [0..25000], [0..25000]]
  forM_ lists $ \list -> (do
    (_, res') <- Th.forkIO $ do
      forM_ list $ \_ -> performRandAction ht
    res <- res'
    Th.result res)

performRandAction :: CHT Int Int -> IO ()
performRandAction ht = do
  op <- randomRIO(False, True)
  if op
  then do
    someKey <- randomRIO(-100000, 100000::Int)
    someValue <- randomRIO(-100000, 100000::Int)
    putCHT someKey someValue ht
  else do
    someKey <- randomRIO(-100000, 100000::Int)
    _ <- getCHT someKey ht
    return ()
