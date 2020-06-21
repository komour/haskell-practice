{-# LANGUAGE InstanceSigs #-}

module Comonad19 where

import           Control.Comonad
import           Control.Concurrent (threadDelay)
import           Control.Monad      (liftM2)
import           System.Process     (callCommand)
import           System.Random

data ListZipper a = LZ [a] a [a]

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

data Status = Incubative
    | Symptomatic
    | Immune
    | Susceptible

data Fellow = Fellow Status Int StdGen Float
-- Int field    - condition (iterations rest to change status)
-- Float field  - random number

instance Show Fellow where
  show (Fellow Susceptible _ _ _) = " "
  show (Fellow Incubative _ _ _)  = "#"
  show (Fellow Symptomatic _ _ _) = "#"
  show (Fellow Immune _ _ _)      = "@"

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
up   (Grid g) = Grid $ listLeft  g
down (Grid g) = Grid $ listRight g

left, right :: Grid a -> Grid a
left  (Grid g) = Grid $ fmap listLeft  g
right (Grid g) = Grid $ fmap listRight g

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
isInfect (Fellow Incubative _ _ _)  = True
isInfect (Fellow Symptomatic _ _ _) = True
isInfect Fellow {}                  = False

infectCount :: [Fellow] -> Int
infectCount = length . filter isInfect

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [left, right]
        verticals   = [up, down]

infectNeighbours :: Grid Fellow -> Int
infectNeighbours g = infectCount $ map (\direction -> extract $ direction g) neighbours

fellowPredicate :: Float -> Fellow -> Bool
fellowPredicate p (Fellow Incubative _ _ n)  = n <= p
fellowPredicate p (Fellow Symptomatic _ _ n) = n <= p
fellowPredicate _ Fellow {}                  = False

decideInfection :: Float -> Grid Fellow -> Bool
decideInfection p g = any (fellowPredicate p . (\ direction -> extract $ direction g)) neighbours

rule :: Float -> Int -> Int -> Int -> Grid Fellow -> Fellow
rule p incub symptom immune g = case infectNeighbours g of
     0 -> case extract g of
            Fellow Susceptible x r n -> Fellow Susceptible x r n
            Fellow Immune 0 r n      -> Fellow Susceptible 0 r n
            Fellow Immune x r n      -> Fellow Immune (x - 1) r n
            Fellow Incubative 0 r _  -> let (a, nr) = getRandPair r in Fellow Symptomatic symptom nr a
            Fellow Incubative x r _  -> let (a, nr) = getRandPair r in Fellow Incubative (x - 1) nr a
            Fellow Symptomatic 0 r n -> Fellow Immune immune r n
            Fellow Symptomatic x r _ -> let (a, nr) = getRandPair r in Fellow Symptomatic (x - 1) nr a
     _ -> case extract g of
            Fellow Immune 0 r n      -> Fellow Susceptible 0 r n
            Fellow Immune x r n      -> Fellow Immune (x - 1) r n
            Fellow Incubative 0 r _  -> let (a, nr) = getRandPair r in Fellow Symptomatic symptom nr a
            Fellow Incubative x r _  -> let (a, nr) = getRandPair r in Fellow Incubative (x - 1) nr a
            Fellow Symptomatic 0 r n -> Fellow Immune immune r n
            Fellow Symptomatic x r n -> Fellow Symptomatic (x - 1) r n
            Fellow Susceptible x r n -> if decideInfection p g then
                                            let (a, nr) = getRandPair r in Fellow Incubative incub nr a
                                        else
                                            Fellow Susceptible x r n

updFellow :: Fellow -> Fellow
updFellow (Fellow s c g _) = let (a, ng) = getRandPair g in Fellow s c ng a

getRandPair :: StdGen -> (Float, StdGen)
getRandPair = randomR (0, 1 :: Float)

evolve :: Float -> Int -> Int -> Int -> Grid Fellow -> Grid Fellow
evolve p inc sym imm = extend $ rule p inc sym imm

initLZ :: IO (ListZipper Fellow)
initLZ = do
  rand <- getStdGen
  let (a, gen) = getRandPair rand
  return $ genericMove updFellow updFellow (Fellow Susceptible 0 gen a) -- TODO ?

listFellow :: ListZipper Fellow -> ListZipper Fellow
listFellow  (LZ a b c) = LZ (fmap updFellow a) (updFellow b) $ fmap updFellow c

duplicateFellow :: ListZipper Fellow -> ListZipper (ListZipper Fellow)
duplicateFellow = genericMove listFellow listFellow

initGridWithInfectCenter :: Int -> IO (Grid Fellow)
initGridWithInfectCenter incub = do
  lz <- initLZ
  let grid = Grid $ duplicateFellow lz
  gen <- getStdGen
  let (a, ng) = getRandPair gen
  let fellow = updFellow (Fellow Incubative incub ng a)
  return $ gridWrite (updFellow fellow) grid

gridToList :: Int -> Grid a -> [[a]]
gridToList n = fmap (toList n) . toList n . unGrid

printList2 :: Show a => [[a]] -> IO ()
printList2 list2 = putStrLn $ concatMap (\list -> concatMap (\e -> show e ++ " ") list ++ "\n") list2

printGrid :: Show a => Int -> Grid a -> IO ()
printGrid n g = printList2 $ gridToList n g

toList :: Int -> ListZipper a -> [a]
toList n (LZ ls x rs) = reverse (take n ls) ++ [x] ++ take n rs

infect :: Float -> Int -> Int -> Int -> Int -> Int -> Grid Fellow -> IO ()
infect p inc sym imm iterations size grid = if iterations == 0 then putStrLn ""
  else do
    threadDelay 500000
    callCommand "tput reset"
    printGrid size grid
    infect p inc sym imm (iterations - 1) size $ evolve p inc sym imm grid

startInfect :: Float -> Int -> Int -> Int -> Int -> Int -> IO ()
startInfect p incub sym imm iter size = do
  grid <- initGridWithInfectCenter incub
  infect p incub sym imm iter size grid

sampleInfect :: IO ()
sampleInfect = startInfect 0.35 1 1 3 20 10

sample :: Int -> IO ()
sample x = startInfect 0.35 1 1 3 x 10