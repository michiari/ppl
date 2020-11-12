module ES20201112 where

-- fmap can be seen as "apply f to all elements of a container/context"
-- fmap (+1) $ Just 42
-- fmap (+1) Nothing
-- fmap (+1) [1..10]

-- but also as a way to lift a unary function, so that it works between functors
-- :t fmap
-- :t fmap (+1)
inc :: (Functor f, Num a) => f a -> f a
inc = fmap (+1)
-- inc $ Just 1
-- inc [1..9]

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

-- But this is not the only way of implementing Applicative for lists
newtype ZipList a = ZL [a] deriving (Eq, Show)

instance Functor ZipList where
  fmap f (ZL xs) = ZL $ map f xs

instance Applicative ZipList where
  pure x = ZL $ repeat x
  (ZL fs) <*> (ZL xs) = ZL $ map (\(f, x) -> f x) (zip fs xs)


-- Let us make binary trees Applicative

-- Begin code from 2020/11/05
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

btmap :: (a -> b) -> BTree a -> BTree b
btmap _ BEmpty = BEmpty
btmap f (BNode x l r) = BNode (f x) (btmap f l) (btmap f r)

instance Functor BTree where
  fmap = btmap
-- End code from 2020/11/05

-- E.g. with the semantics of ZipList
instance Applicative BTree where
  pure x = BNode x (pure x) (pure x)
  BEmpty <*> _ = BEmpty
  _ <*> BEmpty = BEmpty
  (BNode f lf rf) <*> (BNode x lx rx) = BNode (f x) (lf <*> lx) (rf <*> rx)

-- Or also with the semantics of nondeterminism, like plain lists

-- Begin code from 2020/11/05
btfoldr :: (a -> b -> b) -> b -> BTree a -> b
btfoldr _ acc BEmpty = acc
btfoldr f acc (BNode x l r) =
  f x (btfoldr f (btfoldr f acc r) l)

instance Foldable BTree where
  foldr = btfoldr
-- End code from 2020/11/05

-- Concatenation for trees
btcat :: BTree a -> BTree a -> BTree a
btcat BEmpty t = t
btcat (BNode x l r) t = BNode x l (btcat r t)

btconcat :: (Foldable f) => f (BTree a) -> BTree a
btconcat t = foldr btcat BEmpty t

btcatMap :: (Foldable f, Functor f) => (a -> BTree b) -> f a -> BTree b
btcatMap f t = btconcat $ fmap f t

instance Applicative BTree where
  pure x = bleaf x
  fs <*> xs = btcatMap (\f -> fmap f xs) fs


-- Monads

-- What if we have a function that *returns* a value wrapped in a container/context?
apply42 :: (Num b, Ord b) => (a -> b) -> a -> Maybe b
apply42 f x = let s = f x
              in if s > 42 then Just s else Nothing

-- we want to apply a sequence of functions to an initial value,
-- but none of them can return a value lower than 42.
sequence42 x = case apply42 (+12) x of
  Nothing -> Nothing
  Just x' -> case apply42 (\x -> x - 6) x' of
    Nothing -> Nothing
    Just x'' -> apply42 (*2) x''

-- do notation:
sequence42do x = do
  x' <- apply42 (+12) x
  x'' <- apply42 (\x -> x - 6) x'
  apply42 (*2) x''

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   fail :: String -> m a

-- sequence42do gets translated into
sequence42bind x = apply42 (+12) x >>=
  (\x' -> apply42 (\y -> y - 6) x' >>=
  (\x'' -> apply42 (*2) x''))

-- another way of writing it
sequence42bind' x = return 42 >>= apply42 (+12) >>= apply42 (\y -> y - 6) >>= apply42 (*2)

-- the >> operator "discards" its first argument, maintaining and propagating its context.
sequenceDiscard x = do
  apply42 (+5) x
  x' <- apply42 (*2) x
  return x'

sequenceDiscardBind x = apply42 (+5) x >> apply42 (*2) x >>= (\x' -> return x')
