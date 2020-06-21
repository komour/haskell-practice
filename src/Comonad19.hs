{-# LANGUAGE InstanceSigs #-}

module Comonad19 where

import           Control.Comonad
import           Control.Monad   (liftM2)

data ListZipper a = LZ [a] a [a]

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

newtype KekBool = KekBool Bool

instance Show KekBool where
  show (KekBool True)  = "🤓"
  show (KekBool False) = "💀"

instance Functor ListZipper where
  fmap :: (a -> b) -> ListZipper a -> ListZipper b
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f g = Grid $ LZ (fmap f <$> l) (fmap f s) (fmap f <$> r)
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

kekPredicate :: KekBool -> Bool
kekPredicate (KekBool True) = True
kekPredicate (KekBool False) = False

aliveCount :: [KekBool] -> Int
aliveCount = length . filter kekPredicate

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [left, right]
        verticals   = [up, down]

aliveNeighbours :: Grid KekBool -> Int
aliveNeighbours g = aliveCount
                  $ map (\direction -> extract $ direction g) neighbours

rule :: Grid KekBool -> KekBool
rule g = case aliveNeighbours g of -- TODO
     8 -> extract g
     _ -> KekBool False

evolve :: Grid KekBool -> Grid KekBool -- TODO
evolve = extend rule

initBoolLZ :: ListZipper KekBool
initBoolLZ = genericMove id id (KekBool True)

initBoolGridWithFalseCenter :: Grid KekBool
initBoolGridWithFalseCenter = gridWrite (KekBool False) initBoolGrid

initBoolGrid :: Grid KekBool
initBoolGrid = Grid $ duplicate initBoolLZ

gridToList :: Int -> Grid a -> [[a]]
gridToList n = fmap (toList n) . toList n . unGrid

printGrid :: Show a => Int -> Grid a -> IO ()
printGrid n g = putStrLn $ concatMap (\list -> concatMap (\e -> show e ++ " ") list ++ "\n") list2
  where list2 = gridToList n g

toList :: Int -> ListZipper a -> [a]
toList n (LZ ls x rs) = reverse (take n ls) ++ [x] ++ take n rs

-- | Main function to begin infection.
-- Example: infect 11 initBoolGridWithFalseCenter
infect :: Int -> Grid KekBool -> IO ()
infect iterations grid = if iterations == 0 then putStrLn ""
  else do
    printGrid 10 grid
    infect (iterations - 1) $ evolve grid
