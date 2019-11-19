module ES6 where

import qualified Data.Map as M

data BTree a = BEmpty | BNode a (BTree a) (BTree a) deriving Eq

instance (Show a) => Show (BTree a) where
  show BEmpty = "Bempty"
  show (BNode x BEmpty BEmpty) = "BNode " ++ show x
  show (BNode x l r) = "BNode " ++ show x ++ " (" ++ show l ++ ") (" ++ show r ++ ")"

bleaf x = BNode x BEmpty BEmpty

isbleaf (BNode _ BEmpty BEmpty) = True
isbleaf _ = False

btmap :: (a -> b) -> BTree a -> BTree b
btmap _ BEmpty = BEmpty
btmap f (BNode x l r) = BNode (f x) (btmap f l) (btmap f r)

instance Functor BTree where
  fmap = btmap

-- fmap can be seen as "apply f to all elements of a container/context"
-- fmap (+1) $ Just 42
-- fmap (+1) Nothing
-- fmap (+1) [1..10]
-- fmap (+1) (BNode 1 (bleaf 2) (bleaf 3))

-- but also as a way to lift a unary function, so that it works between functors
-- :t fmap
-- :t fmap (+1)
incAll :: (Functor f, Num b) => f b -> f b
incAll = fmap (+1)
-- inc $ Just 1
-- inc [1..9]
-- inc (BNode 1 (bleaf 2) (bleaf 3))

-- class (Functor f) => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- pure encloses something into an applicative in a default way
-- pure 42 :: Maybe Integer
-- pure 42 :: [Integer]

-- <*> takes an applicative containing a function, and applies it to the content of another applicative
-- pure (*2) <*> Just 42
-- pure (*2) <*> [1..10]
-- now we can apply general functions to containers/contexts
-- pure (*) <*> Just 6 <*> Just 7
-- pure (\x y z -> x*y*z) <*> Just 2 <*> Just 3 <*> Just 4
-- pure (\x y z -> x*y*z) <*> Just 2 <*> Nothing <*> Just 4

-- (fmap (\x y z -> x*y*z) (Just 2)) <*> Just 3 <*> Just 4
-- (fmap (\x y z -> x*y*z) (Just 2)) <*> Nothing <*> Just 4

-- To make this simpler, we have <$>:
-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- f <$> x = fmap f x

-- (*) <$> Just 6 <*> Just 7
-- (\x y z -> x*y*z) <$> Just 2 <*> Just 3 <*> Just 4
-- (\x y z -> x*y*z) <$> Just 2 <*> Nothing <*> Just 4

-- With lists:
-- (+2) <$> [1..3]
-- (+) <$> [1..3] <*> [2]
-- (+) <$> [1..3] <*> [1..10]
-- [(+1), (+2), (+3)] <*> [1..10]

-- this is due to the peculiar implementation of Applicative for lists
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concatMap' f l = concat' $ map f l

-- instance Applicative [] where
--   pure x = [x]
--   fs <*> xs = concatMap' (\f -> map f xs) fs

-- But this is not the only way of implementing Applicative for lists
data ZipList a = ZEmpty | ZL a (ZipList a)

instance (Eq a) => Eq (ZipList a) where
  ZEmpty == ZEmpty = True
  ZL x xs == ZL y ys = x == y && xs == ys
  _ == _ = False

showZipList ZEmpty = "[]"
showZipList (ZL x ZEmpty) = "[" ++ show x ++ "]"
showZipList (ZL x xs) = "[" ++ show x ++ "," ++ (drop 1 $ showZipList xs)

instance (Show a) => Show (ZipList a) where
  show l = "ZipList " ++ showZipList l

instance Functor ZipList where
  fmap _ ZEmpty = ZEmpty
  fmap f (ZL x xs) = ZL (f x) $ fmap f xs

toZipList :: [a] -> ZipList a
toZipList [] = ZEmpty
toZipList (x:xs) = ZL x (toZipList xs)

instance Applicative ZipList where
  pure x = ZL x $ pure x
  ZEmpty <*> _ = ZEmpty
  _ <*> ZEmpty = ZEmpty
  ZL f fs <*> ZL y ys = ZL (f y) (fs <*> ys)

-- (toZipList [(+1), (+2), (*3)]) <*> (toZipList [1,2,3])
-- pure (*1) <*> toZipList [1..10] -- meh

-- Let us make binary trees Applicative
btcat BEmpty t2 = t2
btcat t1 BEmpty = t1
btcat t1@(BNode x l r) t2 = BNode x l new_r
  where new_r = if isbleaf t1
                then t2
                else btcat r t2
-- btcat (BNode 1 (bleaf 2) (bleaf 3)) (BNode 4 (bleaf 5) (bleaf 6))

btfoldr _ acc BEmpty = acc
btfoldr f acc (BNode x l r) =
  f x (btfoldr f (btfoldr f acc r) l)

btconcat t = btfoldr btcat BEmpty t

btcatmap f t = btconcat $ fmap f t
-- btcatmap (\x -> BNode x (bleaf x) (bleaf x)) (BNode 1 (bleaf 2) (bleaf 3))

-- instance Applicative BTree where
--   pure = bleaf
--   fs <*> xs = btcatmap (\f -> fmap f xs) fs

-- we can also do it with the Zip semantics
instance Applicative BTree where
  pure x = BNode x (pure x) (pure x)
  BEmpty <*> _ = BEmpty
  _ <*> BEmpty = BEmpty
  BNode f lf rf <*> BNode x lx rx = BNode (f x) (lf <*> lx) (rf <*> rx)


-- An exercise on Data.Map
-- map(personNames, PhoneNumbers)
-- map(PhoneNumber, MobileCarriers)
-- map(MobileCarriers, BillingAddresses)

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = TIM | Vodafone | Wind | Ho deriving (Eq, Show, Ord)


findCarrierBillingAddress ::
  PersonName
  -> M.Map PersonName PhoneNumber
  -> M.Map PhoneNumber MobileCarrier
  -> M.Map MobileCarrier BillingAddress
  -> Maybe BillingAddress
findCarrierBillingAddress person phoneMap carrierMap addressMap =
  case M.lookup person phoneMap of
    Nothing -> Nothing
    Just number ->
      case M.lookup number carrierMap of
        Nothing -> Nothing
        Just carrier -> M.lookup carrier addressMap

findCarrierBillingAddress' person phoneMap carrierMap addressMap =
  do
    number <- M.lookup person phoneMap
    carrier <- M.lookup number carrierMap
    M.lookup carrier addressMap
