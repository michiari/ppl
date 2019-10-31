module ES20191029 where

-- Factorial in Haskell
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

facti :: Integer -> Integer
facti 0 = 1
facti n = n * facti (n-1)
-- Int is a fixed-precision integer type with at least the range [-2^29 .. 2^29-1].
-- Integer are arbitrary-precision integers.

-- Fibonacci in Haskell
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- We can use guards, too
fibg :: Integer -> Integer
fibg n | n == 0 = 0
       | n == 1 = 1
       | otherwise = fibg (n-1) + fibg (n-2)

-- A few functions with lists
myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

empty :: [a] -> Bool
empty [] = True
empty (_:_) = False

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

range :: Integer -> Integer -> [Integer]
range a b = if a > b
  then error "Min > Max"
  else if a < b
       then a : (range (a+1) b)
       else [a]

range2 :: Integer -> Integer -> [Integer]
range2 a b | a > b =  error "Min > Max"
           | a < b = a : (range (a+1) b)
           | otherwise = [a]

-- List comprehensions
rightTriang n = [(a, b, c) | a <- [1..n], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2]

allRightTriang = [(a, b, c) | a <- [1..], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2]

-- We can make an infinite list this way, too.
numsfrom :: Integer -> [Integer]
numsfrom n = n : (numsfrom $ n+1)

-- Alternatively:
numsfrom2 n = [n, n+1 ..]

-- fib 100 is very slow...
fibinf = 0 : 1 : [x + y | (x,y) <- zip fibinf $ tail fibinf
-- try take 10 $ zip fibinf $ tail fibinf
-- try fib 100 and take 100 fibinf

fibb n = fibb' n (0,1)
  where fibb' n (f1, f2) | n == 0 = f1
                         | otherwise = (fibb' $! (n-1)) $! (f2, f1+f2)

-- ($!) f $! x = seq x (f x)

-- Higher order functions
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : (myMap f xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs) = if p x
  then x : myTakeWhile p xs
  else []


data TrafficLight = Red | Green | Yellow deriving (Show, Eq)

-- instance Show TrafficLight where
--   show Red = "Red light"
--   show Yellow = "Yellow light"
--   show Green = "Green light"

-- instance Eq TrafficLight where
--   Red == Red = True
--   Yellow == Yellow = True
--   Green == Green = True
--   _ == _ = False

-- Sum type
data Point = Point Float Float deriving (Eq, Show)

pointx (Point x _) = x
pointy (Point _ y) = y

distance :: Point -> Point -> Float
distance (Point x1 x2) (Point y1 y2) =
  let d1 = x1-y1
      d2 = x2-y2
  in sqrt $ (d1*d1) + (d2*d2)

type TPoint = (Float, Float)

tdistance :: TPoint -> TPoint -> Float
tdistance (x1, x2) (y1, y2) =
  let d1 = x1-y1
      d2 = x2-y2
  in sqrt $ (d1*d1) + (d2*d2)

data APoint = APoint {apx, apy :: Float} deriving (Eq, Show)
