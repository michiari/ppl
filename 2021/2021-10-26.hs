module ES5 where

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- myFoldL f e [1, 2, 3, 4]
-- (f (f (f (f e 1) 2) 3) 4)
myFoldL :: (b -> a -> b) -> b -> [a] -> b
myFoldL _ acc [] = acc
myFoldL f acc (x:xs) = myFoldL f (f acc x) xs

-- myFoldR f e [1, 2, 3, 4]
-- (f 1 (f 2 (f 3 (f 4 e))))
myFoldR :: (a -> b -> b) -> b -> [a] -> b
myFoldR _ acc [] = acc
myFoldR f acc (x:xs) = f x (myFoldR f acc xs)

sumf :: Num a => [a] -> a
sumf = foldl (+) 0

elemf x = foldl (\acc y -> acc || x == y) False

elemf2 x = foldr (\y acc -> x == y || acc) False

filterf p = foldr (\x acc -> if p x then x:acc else acc) []

mapf f = foldr (\x acc -> (f x) : acc) []

appf l1 l2 = foldr (:) l2 l1

-- Binary tree
data BTree a = BNode a (BTree a) (BTree a) | BEmpty

bleaf :: a -> BTree a
bleaf n = BNode n BEmpty BEmpty

myTree = BNode 1 (BNode 2 (bleaf 4) BEmpty) (bleaf 3)

instance Eq a => Eq (BTree a) where
  BEmpty == BEmpty = True
  BNode x1 l1 r1 == BNode x2 l2 r2 = x1 == x2
                                     && l1 == l2
                                     && r1 == r2

instance Show a => Show (BTree a) where
  show BEmpty = ""
  show (BNode x l r) =
    "[" ++ show x
    ++ " " ++ show l
    ++ " " ++ show r ++ "]"

btmap :: (a -> b) -> (BTree a) -> (BTree b)
btmap _ BEmpty = BEmpty
btmap f (BNode x l r) = BNode (f x) (btmap f l) (btmap f r)

instance Functor BTree where
  fmap = btmap


btfoldr :: (a -> b -> b) -> b -> BTree a -> b
btfoldr _ acc BEmpty = acc
btfoldr f acc (BNode x l r) =
  f x (btfoldr f (btfoldr f acc r) l)

instance Foldable BTree where
  foldr = btfoldr


inftree n = BNode n child child
  where child = inftree $ n+1

bttake _ BEmpty = BEmpty
bttake h (BNode x l r)
  | h <= 0 = BEmpty
  | otherwise = BNode x (bttake (h-1) l) (bttake (h-1) r)

