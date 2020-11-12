module ES20201105 where

-- Let's make some data types
data RegionStatus = Yellow | Orange | Red

instance Show RegionStatus where
  show Yellow = "Yellow region"
  show Orange = "Orange region"
  show Red = "Red region"

instance Eq RegionStatus where
  Yellow == Yellow = True
  Orange == Orange = True
  Red == Red = True
  _ == _ = False

instance Ord RegionStatus where
  Yellow <= Yellow = True
  Yellow <= Orange = True
  Yellow <= Red = True
  Orange <= Orange = True
  Orange <= Red = True
  Red <= Red = True
  _ <= _ = False

-- Point in the Cartesian plane
data Point = Point Float Float deriving Eq

pointx :: Point -> Float
pointx (Point x _) = x

pointy :: Point -> Float
pointy (Point _ y) = y

-- Point x y -> (x,y)
instance Show Point where
  show p = "(" ++ show (pointx p) ++ ", " ++ show (pointy p) ++ ")"

distance :: Point -> Point -> Float
distance p1 p2 = let dx = pointx p1 - pointx p2
                     dy = pointy p1 - pointy p2
                 in sqrt $ dx*dx + dy*dy

-- Some other ways of declaring it
data APoint = APoint { apx :: Float, apy :: Float } deriving (Show, Eq)

type TPoint = (Float, Float)

tdistance :: TPoint -> TPoint -> Float
tdistance (x1, y1) (x2, y2) = let dx = x1 - x2
                                  dy = y1 - y2
                              in sqrt $ dx*dx + dy*dy

newtype NPoint = NPoint (Float, Float)


-- Yet another binary tree
data BTree a = BEmpty | BNode a (BTree a) (BTree a)

instance Show a => Show (BTree a) where
  show BEmpty = "Empty"
  show (BNode x BEmpty BEmpty) = "Leaf " ++ show x
  show (BNode x l r) = "Node " ++ show x ++ " [" ++ show l ++ "] [" ++ show r ++ "]"

instance Eq a => Eq (BTree a) where
  BEmpty == BEmpty = True
  (BNode x1 l1 r1) == (BNode x2 l2 r2) =
    x1 == x2 && l1 == l2 && r1 == r2
  _ == _ = False

bleaf :: a -> BTree a
bleaf x = BNode x BEmpty BEmpty

-- zip finctions for lists
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)

myZip :: [a] -> [b] -> [(a,b)]
myZip = myZipWith (,)

-- we can make them for trees as well
btZipWith :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
btZipWith _ BEmpty _ = BEmpty
btZipWith _ _ BEmpty = BEmpty
btZipWith f (BNode x1 l1 r1) (BNode x2 l2 r2) =
  BNode (f x1 x2) (btZipWith f l1 l2) (btZipWith f r1 r2)

btZip :: BTree a -> BTree b -> BTree (a,b)
btZip = btZipWith (,)

-- a map for binary trees
btmap :: (a -> b) -> BTree a -> BTree b
btmap _ BEmpty = BEmpty
btmap f (BNode x l r) = BNode (f x) (btmap f l) (btmap f r)

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


-- foldr for binary trees
btfoldr :: (a -> b -> b) -> b -> BTree a -> b
btfoldr _ acc BEmpty = acc
btfoldr f acc (BNode x l r) =
  f x (btfoldr f (btfoldr f acc r) l)

instance Foldable BTree where
  foldr = btfoldr

toList :: BTree a -> [a]
toList t = foldr (:) [] t

size :: BTree a -> Int
size t = foldr (\_ acc -> acc + 1) 0 t

btelem :: Eq a => a -> BTree a -> Bool
btelem x t = foldr (\y acc -> x == y || acc) False  t

-- Infinite trees
inftree n = BNode n (inftree (n+1)) (inftree (n+1))

bttake :: Int -> BTree a -> BTree a
bttake _ BEmpty = BEmpty
bttake h (BNode x l r) | h == 0 = BEmpty
                       | otherwise = BNode x (bttake (h-1) l) (bttake (h-1) r)
