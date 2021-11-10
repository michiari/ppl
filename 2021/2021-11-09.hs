module ES6 where

inc :: (Functor f, Num b) => f b -> f b
inc = fmap (+1)

-- [(+1), (+2), (+3), (+4), (+5)]

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat xs

concatMap' f l = concat' $ map f l

-- instance Applicative [] where
--   pure x = [x]
--   fs <*> xs = concatMap' (\f -> map f xs) fs

-- [(+2), (*3), (+1)] <*> [1,2,3]
-- [(1+2), (2*3), (3+1)]

data ZipList a = Empty | ZL a (ZipList a) deriving Eq

instance Show a => Show (ZipList a) where
  show xs = "ZipList " ++ showZipList xs
    where showZipList Empty = "[]"
          showZipList (ZL x Empty) = "[" ++ show x ++ "]"
          showZipList (ZL x xs) = "[" ++ show x ++ ","
                                  ++ (drop 1 $ showZipList xs)

toZipList :: [a] -> ZipList a
toZipList [] = Empty
toZipList (x:xs) = ZL x $ toZipList xs

instance Functor ZipList where
  fmap _ Empty = Empty
  fmap f (ZL x xs) = ZL (f x) (fmap f xs)

instance Applicative ZipList where
--  pure x = ZL x Empty
  pure x = ZL x (pure x)
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  ZL f fs <*> ZL x xs = ZL (f x) (fs <*> xs)


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
  _ == _ = False

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

-- instance Applicative BTree where
--   pure x = BNode x (pure x) (pure x)
--   BEmpty <*> _ = BEmpty
--   _ <*> BEmpty = BEmpty
--   BNode f fs1 fs2 <*> BNode x xs1 xs2 =
--     BNode (f x) (fs1 <*> xs1) (fs2 <*> xs2)

btcat BEmpty t = t
btcat t1@(BNode x l r) t2 = BNode x l new_r
  where new_r = case r of
                  BEmpty -> t2
                  _ -> btcat r t2

btconcat t = foldr btcat BEmpty t

btConcatMap :: (a -> BTree b) -> BTree a -> BTree b
btConcatMap f t = btconcat $ fmap f t

instance Applicative BTree where
  pure x = bleaf x
  fs <*> xs = btConcatMap (\f -> fmap f xs) fs
