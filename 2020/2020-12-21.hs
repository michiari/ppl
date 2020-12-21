module ES11 where

import Prelude hiding (Either(..), either)
import Data.Char (digitToInt, isDigit)

-- The Either monad offers more advanced error handling than the Maybe monad
data Either a b = Left a | Right b deriving (Eq, Ord, Show)

-- Some utility functions:
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right x) = g x

rights :: [Either a b] -> [b]
rights l = [y | Right y <- l]

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where left x (l, r) = (x:l, r)
        right x (l, r) = (l, x:r)

-- The relevant instances
instance Functor (Either a) where
  fmap f (Right x) = Right $ f x
  fmap _ (Left x) = Left x

instance Applicative (Either a) where
  pure = Right
  (Left f) <*> _ = Left f
  (Right f) <*> r = fmap f r

instance Monad (Either a) where
  return = pure
  (Right x) >>= f = f x
  (Left x) >>= _ = Left x

apply42 :: (Num b, Ord b) => (a -> b) -> a -> Either b b
apply42 f x = let s = f x
              in if s > 42 then Right s else Left s

sequence42 x = do
  x' <- apply42 (+12) x
  x'' <- apply42 (\x -> x - 6) x'
  apply42 (*2) x''

-- We can use Either to make a parser that tells us what's wrong with our string if parsing fails.
parseEither :: Char -> Either String Int
parseEither c | isDigit c = Right $ digitToInt c
              | otherwise = Left $ "Expected digit, got " ++ [c]

parseMultiple1 :: Either String Int
parseMultiple1 = do
  x <- parseEither '1'
  y <- parseEither '2'
  return (x + y)

parseMultiple2 :: Either String Int
parseMultiple2 = do
  x <- parseEither 'm'
  y <- parseEither '2'
  return (x + y)

parseNumber :: String -> Either String Int
parseNumber sn = foldl parser (Right 0) sn
  where parser acc d = do
          accp <- acc
          dp <- parseEither d
          return $ accp * 10 + dp

-- acc >>= (\accp -> parseEither d >>= (\dp -> return $ accp * 10 + dp))
