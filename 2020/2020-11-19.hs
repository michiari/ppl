module ES20201119 where

import Prelude hiding (log)

type Log = [String]

-- A monad to log sequential operations
data Logger a = Logger { content :: a
                       , log :: Log
                       }

instance Eq a => Eq (Logger a) where
  l1 == l2 = content l1 == content l2

instance Show a => Show (Logger a) where
  show l = show (content l)
    ++ "\n\nLog:"
    ++ foldr (\line acc -> "\n\t" ++ line ++ acc) "" (log l)

instance Functor Logger where
  fmap f l = Logger (f $ content l) (log l)

instance Applicative Logger where
  pure x = Logger x []
  (Logger f lf) <*> (Logger x lx) = Logger (f x) (lf ++ lx)

instance Monad Logger where
  return = pure
  Logger x oldLog >>= f = let Logger y newLog = f x
                          in Logger y (oldLog ++ newLog)

-- Some monadic functions with logging
logIncOne :: Num n => n -> Logger n
logIncOne x = Logger (x+1) ["Increased by 1."]

logMulTwo :: Num n => n -> Logger n
logMulTwo x = Logger (x*2) ["Multiplied by 2."]

doOps x = do
  x1 <- logIncOne x
  x2 <- logMulTwo x1
  logIncOne x2

-- Monadic Laws
-- Left identity: return a >>= f ≡ f a
-- f has the type (a -> m b) so it returns a monad
-- this means that the minimal context to return
-- is just applying f to a

-- return 42 >>= logPlusOne
-- logPlusOne 42

-- Right identity: m >>= return ≡ m
-- When we feed monadic values to functions by using >>=,
-- those functions take normal values and return monadic ones.
-- return is also one such function, if you consider its type.

-- Logger (1,["barnibalbi"]) >>= return

-- Associativity:  (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

-- doOps, doOps', and doOps'' are equivalent because of monadic laws.
doOps' x = logIncOne x >>=
  (\x1 -> logMulTwo x1 >>=
    (\x2 -> logIncOne x2 >>=
      (\x3 -> return x3)))

doOps'' x = logIncOne x >>= logMulTwo >>= logIncOne


putLog :: String -> Logger ()
putLog msg = Logger () [msg]

-- Begin code from 2020/11/05
data BTree a = BEmpty | BNode a (BTree a) (BTree a)

instance Show a => Show (BTree a) where
  show BEmpty = "Empty"
  show (BNode x BEmpty BEmpty) = "Leaf " ++ show x
  show (BNode x l r) = "Node " ++ show x ++ " [" ++ show l ++ "] [" ++ show r ++ "]"

bleaf :: a -> BTree a
bleaf x = BNode x BEmpty BEmpty
-- End code from 2020/11/05

bleafM :: Show a => a -> Logger (BTree a)
bleafM x = do
  putLog $ "Created leaf " ++ show x
  return $ bleaf x

treeReplaceM :: (Eq a, Show a) => BTree a -> a -> a -> Logger (BTree a)
treeReplaceM BEmpty _ _ = return BEmpty
treeReplaceM (BNode x l r) y z = do
  newL <- treeReplaceM l y z
  newR <- treeReplaceM r y z
  if x == y
    then (putLog $ "Replaced " ++ show x ++ " with " ++ show z)
         >> (return $ BNode z newL newR)
    else return $ BNode x newL newR
-- treeReplaceM (BNode 1 (bleaf 2) (bleaf 3)) 2 4

buildTreeM 0 = bleafM 0
buildTreeM n = do
  putLog $ "Added node " ++ show n
  l <- buildTreeM (n `div` 2)
  r <- buildTreeM ((n `div` 2) - (1 - (n `mod` 2)))
  return $ BNode n l r

-- Function that logs whether a tree is unbalanced.
isBalancedM :: BTree a -> Logger (Bool, Int)
isBalancedM BEmpty = do
  putLog "Empty tree: balanced."
  return (True, 0)
isBalancedM (BNode _ l r) = do
  (balancedL, hL) <- isBalancedM l
  (balancedR, hR) <- isBalancedM r
  let hNew = (max hL hR) + 1
  if abs (hL - hR) > 1
    then (putLog "Unbalanced!") >> (return (False, hNew))
    else return (balancedL && balancedR, hNew)
-- isBalancedM (BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (BNode 5 (bleaf 6) (bleaf 7))))
-- isBalancedM (BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5)))


-- Exam 2017/02/27, Exercise 2
data LolStream x = LolStream Int [x]

instance (Show a) => Show (LolStream a) where
  show = show . destream

-- The list [x] must always be an infinite list
-- (also called a stream), while the first parameter, of type Int,
-- when positive represents the fact that the stream is periodic, while it is not periodic if
-- negative (0 is left unspecified). E.g.
-- LolStream -1 [1,2..]
-- LolStream 2 [1,2,1,2...]
period12 = [1,2] ++ period12

isperiodic :: LolStream a -> Bool
isperiodic (LolStream n _) = n > 0

destream lol@(LolStream n l) = if isperiodic lol
                               then take n l
                               else l

lolrepeat :: [a] -> [a]
lolrepeat l = l ++ lolrepeat l

-- Define a function lol2lolstream which takes a finite list of finite lists
-- [h 1 , h 2 , ... h n ], and returns
-- LolStream (|h 1 | + |h 2 | + ... + |h n |) (h 1 ++ h 2 ++ ... ++ h n ++ h 1 ++ h 2 ++ ...)
lol2lolstream :: [[a]] -> LolStream a
lol2lolstream ls = let lscat = concat ls
                   in LolStream (length lscat) (lolrepeat lscat)

-- Please continue the exercise by yourself, and also try Exercise 2 from the exam of 2020/01/15.
-- We will see the solutions next time.
