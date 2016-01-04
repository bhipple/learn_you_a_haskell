import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

main = putStrLn "http://learnyouahaskell.com/for-a-few-monads-more"

-- Writer Monad
-- Used for values that have another value attached that acts as a log
-- We represent this as a (val, writerLog) pair wrapped in a newtype so
-- that it can be declared an instance of Monad
{-
newtype Writer w a = Writer { runWriter :: (a, w) }

-- Note that we restrict the writer type to be a monoid, so that it can be
-- mappend'ed together with new writer events!
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
-}

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = logNumber 3 >>= \x -> logNumber (5*x)

-- The tell function writes something into the monoid without affecting the value
multWithLogTell :: Writer [String] Int
multWithLogTell = do
        a <- logNumber 3
        b <- logNumber 5
        tell ["Gonna mult these two"]
        return (a*b)

-- We use the Writer monad to add logging to our program
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- If the associativity of our appending operations is wrong, we can use
-- a DiffList to improve the efficiency.

-- The Reader Monad
-- Allows us to treat functions as values with a context.
-- Useful when we have a lot of functions that are going to be applied to
-- the same thing.
--
-- Functions (->) are also Monads!
addStuff :: Int -> Int
addStuff = do
        a <- (*2)
        b <- (+10)
        return (a+b)

eq22 = addStuff 4

-- The State Monad
-- A stateful computation is a function that takes some state and returns
-- a value along with some new state, s -> (a,s)
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackSeq :: State Stack Int
stackSeq = do
        push 3
        a <- pop
        pop

stackEx = runState stackSeq [5,8,2,1]

-- State comes with two useful utlity functions:
-- get -> takes a State and returns the current state
-- put -> takes a State and a newstate replaces it with newstate

-- We can also wrap Random in a State monad to avoid having to keep passing
-- it the returned generator explicitly.


-- Error Monad
-- We use the Either type to represent successful computation with a Right
-- value or a failed computation with a Left value and some piece of data
-- explaining the failure. The Left type of the Error Monad has to be an
-- instance of the Error typeclass, which defines a strMsg function.

-- Again, since monads are applicative functors but applicative functors
-- hadn't been invented when monads were, there are a number of redundant
-- functions:
-- pure == return
-- liftM == fmap == (<$>)
-- ap == <*>
--
-- That said, these functiosn can be implemented in terms of Monad operations.
-- So if we're making a new Monad typeclass, we can make it an instance of
-- Applicative by just specifying pure == return and <*> == ap, and make it a
-- functor with fmap == liftM, after defining the Monad functions.
--
-- Also look at liftA2 and liftM2

{-- Join takes values like Just (Just 9) and flattens them to (Just 9),
-- taking into account the semantics of combining values in that monadic context
join' :: (Monad m) => m (m a) -> m a
-}

-- As a tip, for any monad,
-- m >>= f == join (fmap f m)
-- Sometimes it can be easier to figure out how we'd do the join than
-- figure out how we'd implement the >>=

-- FilterM to get Powersets
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])
