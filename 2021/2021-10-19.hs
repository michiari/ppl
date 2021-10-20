module ES4 where

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

facti :: Integer -> Integer
facti 0 = 1
facti n = n * facti (n - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

empty :: [a] -> Bool
empty [] = True
empty _ = False

range :: Integer -> Integer -> [Integer]
range a b = if a > b
            then error "Min > Max"
            else if a < b
                 then a : (range (a+1) b)
                 else [a]

rangeg :: Integer -> Integer -> [Integer]
rangeg a b | a > b = error "Min > Max"
           | a < b = a : (range (a+1) b)
           | otherwise = [a]

rightTriangles n = [(a, b, c) | a <- [1..n], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2]

allRightTriangles = [(a, b, c) | a <- [1..], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2]

numsfrom :: Integer -> [Integer]
numsfrom n = n : (numsfrom $ n+1) -- (numsfrom (n+1))

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n (x:xs)
  | n > 0 = x : (myTake (n-1) xs)
  | otherwise = []

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x) : (myMap f xs)

-- Run-length encoding
-- [1, 1, 3, 3, 3, 4, 5, 5, 1]
-- [[1, 1], [3, 3, 3], [4], [5, 5], [1]]
-- [(1, 2), (3, 3), (4, 1), (5, 2), (1, 1)]

pack :: Eq a => [a] -> [[a]]
pack l = reverse $ packAux l [] []
  where packAux :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
        packAux [] currPack acc = currPack : acc
        packAux (x:xs) [] acc = packAux xs [x] acc
        packAux (x:xs) currPack@(y:ys) acc
          | x == y = packAux xs (x:currPack) acc
          | otherwise = packAux xs [x] (currPack:acc)

rlencode :: Eq a => [a] -> [(a, Int)]
rlencode l = map (\p -> (head p, length p)) $ pack l

-- Sum type
data TrafficLight = Red | Yellow | Green -- deriving (Eq, Show)

instance Eq TrafficLight where
  Green == Green = True
  Yellow == Yellow = True
  Red == Red = True
  _ == _ = False

instance Show TrafficLight where
  show Green = "Go"
  show Yellow = "Go faster"
  show Red = "Stop"

data Point = Point Float Float deriving (Eq, Show)

pointx (Point x _) = x
pointy (Point _ y) = y

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) =
  let dx = x1 - x2
      dy = y1 - y2
  in sqrt $ dx^2 + dy^2

data APoint = APoint { apx, apy :: Float } deriving (Eq, Show)

type TPoint = (Float, Float)

tdistance :: TPoint -> TPoint -> Float
tdistance (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
  in sqrt $ dx^2 + dy^2

tpointx p = fst p

newtype NPoint = NPoint (Float, Float)

npointx :: NPoint -> Float
npointx (NPoint p) = fst p

instance Show NPoint where
  show (NPoint (x,y)) = show $ x + y
