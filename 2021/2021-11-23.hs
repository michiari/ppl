module ES8 where

import Control.Monad.State.Lazy
import Data.List (isInfixOf)

-- Exam 2027/02/27, exercise 2
data LolStream x = LolStream Int [x]

isPeriodic :: LolStream x -> Bool
isPeriodic (LolStream n _) = n > 0

destream :: LolStream x -> [x]
destream lol@(LolStream n l) =
  if isPeriodic lol
  then take n l
  else l

instance (Show x) => Show (LolStream x) where
  show = show . destream
-- show lol = show (destream lol)

lolRepeat :: [a] -> [a]
lolRepeat l = l ++ lolRepeat l

instance (Eq x) => Eq (LolStream x) where
  lol1 == lol2 = destream lol1 == destream lol2

lol2lolstream :: [[x]] -> LolStream x
lol2lolstream ls =
  let lscat = concat ls
  in LolStream (length lscat) (lolRepeat lscat)

instance Foldable LolStream where
  foldr f e lol = foldr f e $ destream lol

instance Functor LolStream where
  fmap f (LolStream n l) =
    LolStream n (map f l)

instance Applicative LolStream where
  pure x = lol2lolstream [[x]]
  lsf@(LolStream nf _) <*> lsx@(LolStream nx _) =
    LolStream (nf * nx)
    (lolRepeat $ destream lsf <*> destream lsx)

instance Monad LolStream where
  return = pure
  ls >>= f = lol2lolstream
    [destream ls >>= \x -> destream (f x)]

asd = do
  x <- lol2lolstream [[1..4]]
  y <- lol2lolstream [[2..5]]
  return (x, y)

asdList = do
  x <- [1..4]
  y <- [2..5]
  return (x, y)


-- Stack with the State Monad
type Stack = [Int]

pop :: Stack -> (Stack, Int)
pop [] = error "Pop on empty stack."
pop (x:xs) = (xs, x)

push :: Stack -> Int -> Stack
push xs x = x:xs

stackManip s0 =
  let (s1, _) = pop s0
      (s2, _) = pop s1
      s3 = push s2 100
      (s4, _) = pop s3
  in push s4 42


popM :: State Stack Int
popM = do
  stack <- get
  case stack of
    (x:xs) -> put xs >> return x
    [] -> error "Pop on empty stack."

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


-- Exam 2020/01/15, exercise 2
data CashRegister a =
  CashRegister { getReceipt :: (a, Float) }
  deriving (Show, Eq)
getCurrentItem = fst . getReceipt
getPrice = snd . getReceipt

instance Functor CashRegister where
  fmap f cr = CashRegister (f $ getCurrentItem cr, getPrice cr)

instance Applicative CashRegister where
  pure x = CashRegister (x, 0.0)
  crf <*> crx = CashRegister
    (getCurrentItem crf $ getCurrentItem crx,
     getPrice crf + getPrice crx)

instance Monad CashRegister where
  return = pure
  cr >>= f =
    let newCR = f $ getCurrentItem cr
    in CashRegister (getCurrentItem newCR,
                     getPrice cr + getPrice newCR)

addItem :: String -> Float -> CashRegister String
addItem item price = CashRegister (item, price)

checkBag :: String -> CashRegister String
checkBag item = if isInfixOf "Vegetables" item
                then CashRegister ("Bag", 0.01)
                else CashRegister ("", 0.0)

buyGroceries = do
  i1 <- addItem "Vegetables: apples" 2.15
  checkBag i1
  i2 <- addItem "Meat: chicken breast" 4.50
  checkBag i2
  i3 <- addItem "Vegetables: tomatoes" 2.30
  checkBag i3
