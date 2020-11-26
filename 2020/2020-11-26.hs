module ES20201126 where

import Data.List (isInfixOf)
import Control.Monad.State.Lazy

-- Exam 2017/02/27, Exercise 2

-- Begin code from 2020/11/19
data LolStream x = LolStream Int [x]

isperiodic :: LolStream a -> Bool
isperiodic (LolStream n _) = n > 0

destream lol@(LolStream n l) = if isperiodic lol
                               then take n l
                               else l

instance (Show a) => Show (LolStream a) where
  show = show . destream

lolrepeat :: [a] -> [a]
lolrepeat l = l ++ lolrepeat l

-- Define a function lol2lolstream which takes a finite list of finite lists
-- [h 1 , h 2 , ... h n ], and returns
-- LolStream (|h 1 | + |h 2 | + ... + |h n |) (h 1 ++ h 2 ++ ... ++ h n ++ h 1 ++ h 2 ++ ...)
lol2lolstream :: [[a]] -> LolStream a
lol2lolstream ls = let lscat = concat ls
                   in LolStream (length lscat) (lolrepeat lscat)
-- End code from 2020/11/19

instance (Eq a) => Eq (LolStream a) where
  l1 == l2 = destream l1 == destream l2

instance Foldable LolStream where
  foldr f e ls = foldr f e $ destream ls

instance Functor LolStream where
  fmap f (LolStream n l) = LolStream n (map f l)

instance Applicative LolStream where
  pure x = lol2lolstream [[x]]
  lsf@(LolStream nf lf) <*> lsx@(LolStream nx lx) =
    LolStream (nf * nx) $ lolrepeat (destream lsf <*> destream lsx)
-- lol2lolstream [[(+2),(*3)]] <*> lol2lolstream [[1..4]]

instance Monad LolStream where
  return = pure
  ls >>= f = lol2lolstream [destream ls >>= \x -> destream (f x)]

something = do
  x <- lol2lolstream [[1..4]]
  y <- lol2lolstream [[2..5]]
  return (x, y)

-- Exam 2020/01/15, Exercise 2
data CashRegister a = CashRegister { getReceipt :: (a, Float) } deriving (Show, Eq)

getCurrentItem = fst . getReceipt
getPrice = snd . getReceipt

instance Functor CashRegister where
  fmap f cr = CashRegister (f $ getCurrentItem cr, getPrice cr)

--fmap :: (a -> b) -> CashRegister a -> CashRegister b

instance Applicative CashRegister where
  pure x = CashRegister (x, 0)
  crf <*> crx = CashRegister (getCurrentItem crf $ getCurrentItem crx, getPrice crf + getPrice crx)

--(<*>) :: CashRegister (a -> b) -> CashRegister a -> CashRegister b

instance Monad CashRegister where
  return = pure
  cr >>= f = let newCR = f $ getCurrentItem cr
             in CashRegister (getCurrentItem newCR, getPrice cr + getPrice newCR)

--(>>=) :: CashRegister a -> (a -> CashRegister b) -> CashRegister b

addItem :: String -> Float -> CashRegister String
addItem item price = CashRegister (item, price)

checkBag :: String -> CashRegister String
checkBag item = if isInfixOf "Vegetables" item
                then CashRegister ("Bag", 0.1)
                else CashRegister ("", 0)

buyGroceries = do
  i1 <- addItem "Vegetables: apple" 2.15
  checkBag i1
  i2 <- addItem "Meat: checken breast" 4.25
  checkBag i2
  i3 <- addItem "Vegetables: tomatoes" 3.0
  checkBag i3


-- Let us try to implement a stack in Haskell
type Stack = [Int]

pop :: Stack -> (Stack, Int)
pop [] = error "Pop on empty stack."
pop (x:xs) = (xs, x)

push :: Stack -> Int -> Stack
push xs x = x:xs

stackManip :: Stack -> Stack
stackManip s0 = let (s1, _) = pop s0
                    (s2, _) = pop s1
                    s3 = push s2 100
                    (s4, _) = pop s3
                in push s4 42
-- stackManip [1..10]

-- Ugly, right?
-- We could use the State Monad.

-- Some signatures:
-- data State st a = State (st -> (st, a))
-- runState :: State st a -> st -> (a, st)
-- put :: st -> State st ()
-- get :: State st st

popM :: State Stack Int
popM = do
  stack <- get
  case stack of
    [] -> error "Pop on empty stack."
    (x:xs) -> put xs >> return x

pushM :: Int -> State Stack ()
pushM x = do
  stack <- get
  put (x:stack)

stackManipM :: State Stack ()
stackManipM = do
  popM
  popM
  pushM 100
  popM
  pushM 42

-- runState stackManipM [1..10]

-- Exam 2015/09/22, Exercise 2
data Bilist a = Bilist [a] [a] deriving (Show, Eq)

bilist_ref :: Bilist a -> Int -> (a, a)
bilist_ref (Bilist l1 l2) i = (l1 !! i, l2 !! i)

-- This works with lists of odd length, too
oddeven :: [a] -> Bilist a
oddeven l = oddevenh l [] []
  where oddevenh [] even odd = Bilist even odd
        oddevenh [x] even odd = Bilist (even ++ [x]) odd
        oddevenh (x:y:xs) even odd = oddevenh xs (even ++ [x]) (odd ++ [y])

inv_oddeven :: Bilist a -> [a]
inv_oddeven (Bilist l1 l2) = concat $ map (\(x, y) -> [x, y]) $ zip l1 l2

-- Please finish it by yourself, and also make Bilist an instance of Functor, Applicative, and Monad.
