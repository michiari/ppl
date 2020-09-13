module ES8 where

import Control.Monad.Fail

-- Exam 2017 02 27
data LolStream x = LolStream Int [x]

-- The list [x] must always be an infinite list
-- (also called a stream), while the first parameter, of type Int,
-- when positive represents the fact that the stream is periodic, while it is not periodic if
-- negative (0 is left unspecified). E.g.
-- LolStream -1 [1,2..]
-- LolStream 2 [1,2,1,2...]
period12 = [1,2] ++ period12
-- LolStream 2 period12

isperiodic (LolStream n _) = n > 0
destream ls@(LolStream n l) = if isperiodic ls
  then take n l
  else l

instance (Eq a) => Eq (LolStream a) where
  ls1 == ls2 = destream ls1 == destream ls2

instance (Show a) => Show (LolStream a) where
  show = show . destream

-- Define a function lol2lolstream which takes a finite list of finite lists
-- [h 1 , h 2 , ... h n ], and returns
-- LolStream (|h 1 | + |h 2 | + ... + |h n |) (h 1 ++ h 2 ++ ... ++ h n ++ h 1 ++ h 2 ++ ...)
lolrepeat xs = xs ++ lolrepeat xs

lol2lolstream :: [[a]] -> LolStream a
lol2lolstream ls = let lscat = foldr (++) [] ls
                   in LolStream (length lscat) (lolrepeat lscat)

instance Functor LolStream where
  fmap f (LolStream n l) = LolStream n $ map f l

instance Foldable LolStream where
  foldr f e ls = foldr f e $ destream ls

instance Applicative LolStream where
  pure x = lol2lolstream [[x]]
  ls1@(LolStream nf fs) <*> ls2@(LolStream nx xs) =
    LolStream (nf * nx) $ lolrepeat (destream ls1 <*> destream ls2)

-- lol2lolstream [[(+2),(*3)]] <*> lol2lolstream [[1..4]]

instance Monad LolStream where
  ls >>= f = lol2lolstream [destream ls >>= \x -> destream (f x)]


something = do
  x <- lol2lolstream [[1..3]]
  y <- lol2lolstream [[2..4]]
  return (x,y)

somethingButWithLists = do
  x <- [1..3]
  y <- [2..4]
  return (x,y)


-- Let us try to implement a stack in Haskell.
type Stack = [Int]

pop :: Stack -> (Stack, Int)
pop [] = error "Popping an empty stack!"
pop (x:xs) = (xs, x)

push :: Int -> Stack -> (Stack, ())
push x xs = (x:xs, ())

-- Define a function that executes the following operations on the stack:
-- pop an element
-- pop another element
-- push 100
-- pop an element
-- push 42
stackManip :: Stack -> (Stack, ())
stackManip stack = let
  (newStack1, a) = pop stack
  (newStack2, b) = pop newStack1
  (newStack3, ()) = push 100 newStack2
  (newStack4, c) = pop newStack3
  in push 42 newStack4
-- try stackManip [1..10]

data State st a = State (st -> (st, a))

instance Functor (State st) where
  fmap f (State g) = State (\s -> let (s', x) = g s
                                  in  (s', f x))

instance Applicative (State st) where
  pure x = State (\t -> (t, x))
  (State f) <*> (State g) =
    State (\state -> let (s, f') = f state
                         (s', x) = g s
                     in  (s', f' x))

instance Monad (State state) where
  State f >>= g = State (\olds ->
                           let (news, value) = f olds
                               State f' = g value
                           in f' news)

-- We need this to use the do notation with pattern matching since GHC 8.6.1.
instance MonadFail (State a) where
  fail s = error s

runStateM :: State state a -> state -> (state, a)
runStateM (State f) st = f st

getState = State (\state -> (state, state))
putState new = State (\_ -> (new, ()))

-- define pop using the State monad
popM :: State Stack Int
popM = do
  (x:xs) <- getState
  putState xs
  return x

-- define push using the State monad
pushM :: Int -> State Stack ()
pushM x = do
  xs <- getState
  putState (x:xs)
  return ()

stackManipM :: State Stack ()
stackManipM = do
  popM
  popM
  pushM 100
  popM
  pushM 42

-- runStateM stackManipM [1..10]


-- Exam 2015 09 22
-- Define the Bilist data-type, which is a container of two homogeneous lists.
-- Define an accessor for Blist, called bilist_ref, that, given an index i, 
-- returns the pair of values at position i in both lists.
-- E.g. bilist_ref (Bilist [1,2,3] [4,5,6]) 1 should return (2,5).
data Bilist a = Bilist [a] [a] deriving (Eq, Show)

bilist_ref (Bilist l1 l2) n = (l1 !! n, l2 !! n)

-- Define a function, called oddeven, that is used to build a Bilist x y from a simple list. 
-- oddeven takes all the elements at odd positions and puts them in y, 
-- while all the other elements are put in x, maintaining their order. 
-- You may assume that the given list has an even length (or 0). 
-- Write also all the types of the functions you define.
-- E.g. oddeven [1,2,3,4] must be Bilist [1,3] [2,4].
oddeven :: [a] -> Bilist a
oddeven l = oddevenh l [] []
  where oddevenh [] ev od = Bilist ev od
        oddevenh (x:xs) ev od = oddevenh xs od (ev ++ [x])

inv_oddeven :: Bilist a -> [a]
inv_oddeven (Bilist l r) =  concat $ map (\(x,y) -> [x,y]) $ zip l r


bilist_maxh :: (Num a, Ord a) => Bilist a -> Int -> a -> Int -> Int
bilist_maxh (Bilist (l:ls) (r:rs)) pos max maxpos
  | l+r > max = bilist_maxh (Bilist ls rs) (pos+1) (l+r) pos
  | otherwise = bilist_maxh (Bilist ls rs) (pos+1) max maxpos
bilist_maxh _ _ _ maxpos = maxpos

bilist_max (Bilist (l:ls) (r:rs)) =
  bilist_maxh (Bilist ls rs) 1 (l+r) 0

-- Try to make Bilist an instance of Functor, Foldable, Applicative, Monad.
