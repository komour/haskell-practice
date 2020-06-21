{-# LANGUAGE InstanceSigs #-}

module Comonad19 where

import           Control.Comonad
import           Control.Concurrent (threadDelay)
import           Control.Monad      (liftM2)
import           System.Process     (callCommand)
--import           System.Random

data ListZipper a = LZ [a] a [a]

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

data Status = Incubative
    | Symptomatic
    | Immune
    | Susceptible

data Fellow = Fellow Status Int

instance Show Fellow where
  show (Fellow Susceptible _) = " "
  show (Fellow Incubative _)  = "#"
  show (Fellow Symptomatic _) = "#"
  show (Fellow Immune _)      = "@"

instance Functor ListZipper where
  fmap :: (a -> b) -> ListZipper a -> ListZipper b
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f g = Grid $ LZ (fmap f <$> l) (fmap f s) $ fmap f <$> r
    where
      LZ l s r = unGrid g

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead

  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical

incubDays, symptomDays, immunDays :: Int
incubDays = 3
symptomDays = 3
immunDays = 3

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper a
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

listLeft :: ListZipper a -> ListZipper a
listLeft  (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _                 = error "listLeft"

listRight :: ListZipper a -> ListZipper a
listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _                = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)

left, right :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical   = genericMove up   down

isInfect :: Fellow -> Bool
isInfect (Fellow Incubative _)  = True
isInfect (Fellow Symptomatic _) = True
isInfect (Fellow _ _)           = False

infectCount :: [Fellow] -> Int
infectCount = length . filter isInfect

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [left, right]
        verticals   = [up, down]

infectNeighbours :: Grid Fellow -> Int
infectNeighbours g = infectCount
                  $ map (\direction -> extract $ direction g) neighbours

rule :: Double -> Int -> Int -> Int -> Grid Fellow -> Fellow
rule p incub symptom immune g = case infectNeighbours g of -- TODO
     0 -> case extract g of
            Fellow Susceptible x -> Fellow Susceptible x
            Fellow Immune 0      -> Fellow Susceptible 0
            Fellow Immune x      -> Fellow Immune $ x - 1
            Fellow Incubative 0  -> Fellow Symptomatic symptom
            Fellow Incubative x  -> Fellow Incubative $ x - 1
            Fellow Symptomatic 0 -> Fellow Immune immune
            Fellow Symptomatic x -> Fellow Symptomatic $ x - 1
     _ -> case extract g of
            Fellow Susceptible _ -> Fellow Incubative incub
            Fellow Immune 0      -> Fellow Susceptible 0
            Fellow Immune x      -> Fellow Immune $ x - 1
            Fellow Incubative 0  -> Fellow Symptomatic symptom
            Fellow Incubative x  -> Fellow Incubative $ x - 1
            Fellow Symptomatic 0 -> Fellow Immune immune
            Fellow Symptomatic x -> Fellow Symptomatic $ x - 1

evolve :: Double -> Int -> Int -> Int -> Grid Fellow -> Grid Fellow
evolve p inc sym imm = extend $ rule p inc sym imm

initLZ :: ListZipper Fellow
initLZ = genericMove id id (Fellow Susceptible 0)

initGrid :: Grid Fellow
initGrid = Grid $ duplicate initLZ

initGridWithInfectCenter :: Int -> Grid Fellow
initGridWithInfectCenter k = gridWrite (Fellow Incubative k) initGrid

gridToList :: Int -> Grid a -> [[a]]
gridToList n = fmap (toList n) . toList n . unGrid

printGrid :: Show a => Int -> Grid a -> IO ()
printGrid n g = putStrLn $ concatMap (\list -> concatMap (\e -> show e ++ " ") list ++ "\n") list2
  where list2 = gridToList n g

toList :: Int -> ListZipper a -> [a]
toList n (LZ ls x rs) = reverse (take n ls) ++ [x] ++ take n rs

infect :: Double -> Int -> Int -> Int -> Int -> Int -> Grid Fellow -> IO ()
infect p inc sym imm iterations size grid = if iterations == 0 then putStrLn ""
  else do
    threadDelay 500000
    callCommand "tput reset"
    printGrid size grid
    infect p inc sym imm (iterations - 1) size $ evolve p inc sym imm grid

startInfect :: Double -> Int -> Int -> Int -> Int -> Int -> IO ()
startInfect p inc sym imm iter size = infect p inc sym imm iter size $ initGridWithInfectCenter inc

sampleInfect :: IO ()
sampleInfect = startInfect 0.5 1 1 3 20 10

sample :: Int -> IO ()
sample x = startInfect 0.5 1 1 3 x 10