module ES20201022 where


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
fib n = fib (n - 1) + fib (n - 2)

-- We can use guards, too
fibg :: Integer -> Integer
fibg n | n == 0 = 0
       | n == 1 = 1
       | otherwise = fibg (n - 1) + fibg (n - 2)


-- A few functions with lists
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

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

-- List comprehensions
rightTriangles :: Integer -> [(Integer, Integer, Integer)]
rightTriangles n = [(a, b, c) | a <- [1..n], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2]

allRightTriangles :: [(Integer, Integer, Integer)]
allRightTriangles = [(a, b, c) | a <- [1..], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2]

-- We can make an infinite list this way, too.
numsfrom :: Integer -> [Integer]
numsfrom n = n : (numsfrom $ n + 1)

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : (myTake (n-1) xs)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x) : (myMap f xs)


-- [1, 2, 3, 3, 3, 4, 5, 5, 6]
-- [[1], [2], [3, 3, 3], [4], [5, 5], [6]]
-- [(1, 1), (2, 1), (3, 3), (4, 1), (5, 2), (6, 1)]

-- Pack equal elements
pack :: Eq a => [a] -> [[a]]
pack lst = reverse $ packAux lst [] []
  where packAux :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
        packAux [] currPack acc = currPack : acc
        packAux (x:xs) [] acc = packAux xs [x] acc
        packAux (x:xs) currPack@(y:ys) acc
          | x == y = packAux xs (x:currPack) acc
          | otherwise = packAux (x:xs) [] (currPack:acc)

-- Run-length encoding
rencode :: Eq a => [a] -> [(a, Int)]
rencode l = map encode $ pack l
  where encode p = (head p, length p)

-- Folds
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (y:ys) = myFoldl f (f acc y) ys

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (y:ys) = f y (myFoldr f acc ys)

-- Folds can be used to quickly implement many functions.
mapf :: (a -> b) -> [a] -> [b]
mapf f = myFoldr (\x acc -> (f x):acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x then x:acc else acc) []
