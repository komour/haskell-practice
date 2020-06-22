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

infectPredicate :: Float -> Fellow -> Bool
infectPredicate p (Fellow _ _ _ n)  = n <= p

-- | Generate list of the given size updating StdGen in each next Fellow
getRandList :: Int -> Fellow -> [Fellow]
getRandList 0 (Fellow s c gen _) = let (newN, newGen) = getRandPairSnd gen in [Fellow s c newGen newN]
getRandList k fel@(Fellow s c gen _) = fel : getRandList (k - 1) (Fellow s c newGen newN)
  where
    (newN, newGen) = getRandPairSnd gen

-- | Change status of the given Fellow to Incubative with given days
infectFellow :: Fellow -> Int -> Fellow
infectFellow (Fellow _ _ g n) incub = Fellow Incubative incub g n

-- | Decide infection of the given Fellow considering
-- count of infected neighbours and probability of infection.
-- p.s. k can't be greater than 8 so imho we can use `last` and `init` functions
decideInfection :: Float -> Int -> Fellow -> Int -> Fellow
decideInfection p k fel incub =  let list' = tail $ getRandList (k + 1) fel in
  let newFel = last list' in
  let list = init list' in
  if any (infectPredicate p) list
  then infectFellow newFel incub
  else newFel

rule :: Float -> Int -> Int -> Int -> Grid Fellow -> Fellow
rule p incub symptom immune g = case infectNeighbours g of
     0 -> case extract g of
            Fellow Susceptible x r n -> Fellow Susceptible x r n
            Fellow Immune 0 r n      -> Fellow Susceptible 0 r n
            Fellow Immune x r n      -> Fellow Immune (x - 1) r n
            Fellow Incubative 0 r n  -> Fellow Symptomatic symptom r n
            Fellow Incubative x r n  -> Fellow Incubative (x - 1) r n
            Fellow Symptomatic 0 r n -> Fellow Immune immune r n
            Fellow Symptomatic x r n -> Fellow Symptomatic (x - 1) r n
     k -> case extract g of
            Fellow Immune 0 r n      -> Fellow Susceptible 0 r n
            Fellow Immune x r n      -> Fellow Immune (x - 1) r n
            Fellow Incubative 0 r n  -> Fellow Symptomatic symptom r n
            Fellow Incubative x r n  -> Fellow Incubative (x - 1) r n
            Fellow Symptomatic 0 r n -> Fellow Immune immune r n
            Fellow Symptomatic x r n -> Fellow Symptomatic (x - 1) r n
            Fellow Susceptible x r n -> decideInfection p k (Fellow Susceptible x r n) incub

getRandPairFst :: StdGen -> (Float, StdGen)
getRandPairFst gen = randomR (0, 1 :: Float) newGen
  where
    newGen = fst $ split gen

getRandPairSnd :: StdGen -> (Float, StdGen)
getRandPairSnd gen = randomR (0, 1 :: Float) newGen
  where
    newGen = snd $ split gen

evolve :: Float -> Int -> Int -> Int -> Grid Fellow -> Grid Fellow
evolve p inc sym imm = extend $ rule p inc sym imm

-- | Return ListZipper containing (pseudo)random Fellows
initLZ :: IO (ListZipper Fellow)
initLZ = do
  r1 <- newStdGen
  r2 <- newStdGen
  r3 <- newStdGen
  let listL = iterateTailRand r1 $ Fellow Susceptible 0 r1 0
  let listR = iterateTailRand r3 $ Fellow Susceptible 0 r3 0
  let (n, g) = getRandPairFst r2
  let mid = Fellow Susceptible 0 g n
  return $ LZ listL mid listR

-- | Return infinite list of Fellows using given StdGen
iterateTailRand :: StdGen -> Fellow -> [Fellow]
iterateTailRand gen = tail . iterateRand gen

-- | Helper function only for `iterateTailRand`
iterateRand :: StdGen -> Fellow -> [Fellow]
iterateRand gen fel@(Fellow s c _ _) = fel : iterateRand newGen (Fellow s c newGen newN)
  where
    (newN, newGen) = getRandPairFst gen

-- | Update given Fellow with its StdGen
updFellowGenFst, updFellowGenSnd :: Fellow -> Fellow
updFellowGenFst (Fellow s c gen _) = let (newN, newGen) = getRandPairFst gen in Fellow s c newGen newN
updFellowGenSnd (Fellow s c gen _) = let (newN, newGen) = getRandPairSnd gen in Fellow s c newGen newN

-- | `listLeft` and `listRight` alternative for Fellows
listFellowFst, listFellowSnd :: ListZipper Fellow -> ListZipper Fellow
listFellowFst (LZ a b c) = LZ (fmap updFellowGenFst a) (updFellowGenFst b) $ fmap updFellowGenFst c
listFellowSnd (LZ a b c) = LZ (fmap updFellowGenSnd a) (updFellowGenSnd b) $ fmap updFellowGenSnd c

--zipperSquared :: ListZipper Fellow -> IO (ListZipper (ListZipper Fellow))
--zipperSquared lz = return $ genericMove listFellow listFellow lz

initGridWithInfectCenter :: Int -> IO (Grid Fellow)
initGridWithInfectCenter incub = do
  lz <- initLZ
  let zipSq = genericMove listFellowFst listFellowSnd lz
  let grid = Grid zipSq
  gen <- newStdGen
  let (newN, newGen) = getRandPairFst gen
  return $ gridWrite (Fellow Incubative incub newGen newN) grid

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
sampleInfect = startInfect 0.1 5 5 10 100 10

sample :: Int -> IO ()
sample x = startInfect 0.35 1 1 3 x 10
