module ES5 where

-- A few more higher order functions

-- map
-- map (+1) [1..10]

-- filter
-- filter even [1..100]
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs) | p x = x : myFilter p xs
                  | otherwise = myFilter p xs

-- zip
-- zip [1..10] ['a'..'j']
myZip :: [a] -> [b] -> [(a, b)]
myZip l [] = []
myZip [] l = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- myZip [1,2,3] ['a','b','c']

-- zipWith
-- zipWith (*) [1..10] [1..10]

myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myFoldL :: (b -> a -> b) -> b -> [a] -> b
myFoldL _ acc [] = acc
myFoldL f acc (x:xs) = myFoldL f (f acc x) xs
-- myFoldL (+) 0 [1,2,3]

myFoldR :: (a -> b -> b) -> b -> [a] -> b
myFoldR _ acc [] = acc
myFoldR f acc (x:xs) = f x $ myFoldR f acc xs

-- we can use folds to redefine many higher order functions
sumf :: Num n => [n] -> n
sumf = foldl (+) 0

elem' e = foldl (\acc x -> x == e || acc) False
-- what's the type of elem'?
-- elem' 2 [1..10]
-- elem' 100 [1..10]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr
  (\x acc -> if p x then x : acc else acc)
  []
-- filterf odd [1..10]

map' f = foldr (\x acc -> (f x): acc) []

map'' f = foldl (\acc x -> acc ++ [f x]) []
-- map'' (+3) [1..10]

-- Try foldr (:) [] [1..10]
-- this is the identity

lapp :: [a] -> [a] -> [a]
lapp l1 l2 = foldr (:) l2 l1
-- lapp [1..10] [20..100]
-- [1..10] ++ [20..100]

takeWhile' p = foldr (\x acc -> if p x then x:acc else []) []
-- it works with infinite lists too
-- takeWhile' (<10) [1..]

-- Binary Tree
data BTree a = BEmpty | BNode a (BTree a) (BTree a)

instance Eq a => Eq (BTree a) where
  BEmpty == BEmpty = True
  BNode x1 l1 r1 == BNode x2 l2 r2 =
    x1 == x2 && l1 == l2 && r1 == r2
  _ == _ = False

instance Show a => Show (BTree a) where
  show BEmpty = "Empty"
  show (BNode x BEmpty BEmpty) = "BNode " ++ show x
  show (BNode x l r) = "BNode " ++ show x ++ " (" ++ show l ++ ") (" ++ show r ++ ")"

bleaf x = BNode x BEmpty BEmpty

isbleaf (BNode _ BEmpty BEmpty) = True
isbleaf _ = False

btmap :: (a -> b) -> BTree a -> BTree b
btmap _ BEmpty = BEmpty
btmap f (BNode x l r) =
  BNode (f x) (btmap f l) (btmap f r)
-- btmap (*2) (BNode 1 BEmpty BEmpty)
-- btmap (*2) (BNode 1 (bleaf 2) (bleaf 3))

instance Functor BTree where
  fmap = btmap

-- Functor laws:
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g
-- Indeed:
-- fmap id (BNode 1 BEmpty BEmpty)
-- fmap id (BNode 1 (bleaf 2) (bleaf 3))
-- and
-- fmap (*2) $ fmap (+1) (BNode 1 (bleaf 2) (bleaf 3))
-- fmap ((*2) . (+1)) (BNode 1 (bleaf 2) (bleaf 3))

btfoldr :: (a -> b -> b) -> b -> BTree a -> b
btfoldr _ acc BEmpty = acc
btfoldr f acc (BNode x l r) =
  f x (btfoldr f (btfoldr f acc r) l)
-- btfoldr (+) 0 (BNode 6 (bleaf 1) (bleaf 2))

instance Foldable BTree where
  foldr = btfoldr

-- Count nodes:
-- foldr (\_ acc -> acc + 1) 0 (BNode 6 (bleaf 1) (bleaf 2))

-- Depth-First Visit:
-- foldr (:) [] (BNode 6 (bleaf 1) (bleaf 2))

-- DFS:
btelem x = foldr (\y acc -> x == y || acc) False

-- infinite trees
inftree n = BNode n (inftree (n+1)) (inftree (n+1))

bttake _ BEmpty = BEmpty
bttake h (BNode x l r)
  | h <= 0 = BEmpty
  | otherwise = BNode x (bttake (h-1) l) (bttake (h-1) r)

btZipWith _ _ BEmpty = BEmpty
btZipWith _ BEmpty _ = BEmpty
btZipWith f (BNode x1 l1 r1) (BNode x2 l2 r2) =
  BNode (f x1 x2) (btZipWith f l1 l2) (btZipWith f r1 r2)
