module Main where
-- We use Main as the module name so that we can compile this to
-- a stand-alone executable with
-- ghc --make 2021-11-16.hs

fact :: Integer -> Integer
fact 0 = 1
fact n = n * (fact $ n-1)

main :: IO ()
-- main = putStrLn "Hello world!"
main = do
  putStrLn "Write a number: "
  numStr <- getLine
  let n = read numStr :: Integer
      f = fact n
  putStr (show n ++ "! = ")
  print f -- putStr (show f)

apply42 :: (Ord a, Num a)
  => (p -> a) -> p -> Maybe a
apply42 f x = let s = f x
              in if s > 42
                 then Just s
                 else Nothing

sequence42 x =
  case apply42 (+12) x of
    Nothing -> Nothing
    Just x' -> case apply42 (\x -> x-6) x' of
                 Nothing -> Nothing
                 Just x'' -> apply42 (*2) x''

sequence42' x = return x -- Just x
  >>= apply42 (+12)
  >>= apply42 (\x -> x-6)
  >>= apply42 (*2)

sequence42do x = do
  x1 <- apply42 (+12) x
  x2 <- apply42 (\x -> x-6) x1
  x3 <- apply42 (*2) x2
  return x3

sequence42doDesugared x =
  apply42 (+12) x
  >>= (\x1 -> apply42 (\x -> x-6) x1
        >>= (\x2 -> apply42 (*2) x2
              >>= (\x3 -> return x3)))

sequenceDiscard x = do
  apply42 (+42) x
  x1 <- apply42 (*2) x
  return x1

sequenceDiscardDesugared x =
  apply42 (+42) x
  >> apply42 (*2) x
  >>= (\x1 -> return x1)

-- Logger Monad
type Log = [String]
data Logger a = Logger { getContent :: a
                       , getLog :: Log
                       }

instance Eq a => Eq (Logger a) where
  l1 == l2 = getContent l1 == getContent l2

instance Show a => Show (Logger a) where
  show l = (show $ getContent l)
    ++ "\n\nLog:"
    ++ foldr (\line acc -> "\n\t"
               ++ line ++ acc) "" (getLog l)

-- (a -> b) -> Logger a -> Logger b
instance Functor Logger where
  fmap f l =
    Logger (f $ getContent l) (getLog l)

instance Applicative Logger where
  pure x = Logger x []
  (Logger f lf) <*> (Logger x lx) =
    Logger (f x) (lf ++ lx)

instance Monad Logger where
  return = pure
  Logger x l >>= f =
    let Logger y newLog = f x
    in Logger y (l ++ newLog)

logPlusOne :: (Num n) => n -> Logger n
logPlusOne x = Logger (x+1) ["Increase by 1."]

logMulTwo :: (Num n) => n -> Logger n
logMulTwo x = Logger (x*2) ["Multiply by 2."]

doOps x = do
  x1 <- logPlusOne x
  x2 <- logMulTwo x1
  x3 <- logPlusOne x2
  return x3

doOps' x = logPlusOne x
  >>= logMulTwo
  >>= logPlusOne
  >>= return

putLog :: String -> Logger ()
putLog msg = Logger () [msg]


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

bleafM x = do
  putLog $ "Created leaf " ++ show x
  return $ bleaf x

treeReplaceM BEmpty _ _ = return BEmpty
treeReplaceM (BNode x l r) repl replWith = do
  newL <- treeReplaceM l repl replWith
  newR <- treeReplaceM r repl replWith
  if x == repl
    then do
    putLog $ "Replaced " ++ show x ++ " with " ++ show replWith
    return $ BNode replWith newL newR
    else return $ BNode x newL newR

buildTreeM :: (Eq n, Show n, Integral n)
           => n -> Logger (BTree n)
buildTreeM 0 = bleafM 0
buildTreeM x = do
  putLog $ "Added node " ++ show x
  l <- buildTreeM (x `div` 2)
  r <- buildTreeM ((x `div` 2) - (1 - (x `mod` 2)))
  return $ BNode x l r
