module ES11 where

import Prelude hiding (Either(..), either)

-- Exam 2015/09/22, Esercise 2
data Bilist a = Bilist [a] [a] deriving (Eq, Show)

bilist_ref :: Bilist a -> Int -> (a, a)
bilist_ref (Bilist l1 l2) i = (l1 !! i, l2 !! i)

oddeven :: [a] -> Bilist a
oddeven l = oddevenh l [] []
  where oddevenh [] even odd = Bilist even odd
        oddevenh el@[x] even odd = Bilist (even ++ el) odd
        oddevenh (x:y:xs) even odd =
          oddevenh xs (even ++ [x]) (odd ++ [y])

inv_oddeven :: Bilist a -> [a]
inv_oddeven (Bilist l1 l2) =
 concat $ map (\(x, y) -> [x, y]) $ zip l1 l2

bilist_max :: (Num n, Ord n) => Bilist n -> Int
bilist_max (Bilist (x:xs) (y:ys)) =
  bilist_maxh (Bilist xs ys) (x+y) 0 1
  where bilist_maxh (Bilist (x:xs) (y:ys)) max maxpos pos =
          let s = x + y
          in if s > max
          then bilist_maxh (Bilist xs ys) s pos (pos + 1)
          else bilist_maxh (Bilist xs ys) max maxpos (pos+1)
        bilist_maxh _ _ maxpos _ = maxpos

instance Functor Bilist where
  -- fmap :: (a -> b) -> Bilist a -> Bilist b
  fmap f (Bilist l1 l2) = Bilist (map f l1) (map f l2)

instance Applicative Bilist where
  pure x = Bilist [x] [x]
  (Bilist f1 f2) <*> (Bilist x1 x2) =
    Bilist (f1 <*> x1) (f2 <*> x2)

bilistcat (Bilist l1 l2) (Bilist r1 r2) =
  Bilist (l1 ++ r1) (l2 ++ r2)

bilistconcat :: [Bilist a] -> Bilist a
bilistconcat bl = foldr bilistcat (Bilist [] []) bl

instance Monad Bilist where
  return = pure
  Bilist l r >>= f = bilistconcat $ (map f l) ++ (map f r)

bldouble n = Bilist [n*2] [n*2+1]


-- Either Monad
data Either a b = Left a | Right b deriving (Eq, Ord, Show)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right x) = g x

instance Functor (Either a) where
  -- fmap :: (b -> c) -> Either a b -> Either a c
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right (f x)

instance Applicative (Either a) where
  pure = Right
  (Left x) <*> _ = Left x
  (Right f) <*> r = fmap f r

instance Monad (Either a) where
  return = pure
  (Right x) >>= f = f x
  (Left x) >>= _ = Left x

apply42 f x = let s = f x
              in if s > 42 then Right s else Left s

sequence42 x = do
  x' <- apply42 (+12) x
  x'' <- apply42 (\x -> x - 6) x'
  apply42 (*2) x''
