import Control.Monad

main = putStrLn "http://learnyouahaskell.com/a-fistful-of-monads"

-- Monads are beefed up applicative functors, in the same way applicative
-- functors are beefed up functors. Monads are applicative functors that
-- also have a bind operator, >>=

-- Our primary motivation: We have a value with some context (Just 'a'),
-- and we want to pass it into a function that takes a normal value (Char)
-- and returns a value with some other context.
{-
- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-}
-- i.e., we have a fancy value and we want to pass it to a function that
-- takes a normal value and returns a fancy value.

-- The Maybe Monad
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- Examples of application:
just4 = Just 3 `applyMaybe` \x -> Just (x+1)
justSmile = Just "smile" `applyMaybe` \x -> Just (x ++ " :)")
nothing = Nothing `applyMaybe` \x -> Just (x ++ " :)")

-- We end up with nothing if the computation fails on either side
just3 = Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing
nothing2 = Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing

-- The Monad typeclass
{-
class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

  fail :: String -> m a
  fail msg = error msg
-}
-- Applicative Functors didn't exist when Haskell and Monads were invented,
-- so it's not explicitly stated on the class declaration, but all Monads are
-- applicative functors.
--
-- return == pure. Keep in mind that it doesn't return from the function; it
-- just takes a value and puts it in the minimal monad context.
--
-- >>= takes a monadic value and feeds it to a function that takes a normal
-- value and returns a monadic value.
--
-- >> comes with a default implementation and is rarely implemented
--
-- We'll discuss fail later, but it's never used explicitly in application code

{-- Implementation of Maybe monad
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing
-}

just90 = Just 9 >>= \x -> return (x*10)

-- Running example with Pierre the tightrope walker
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise = Nothing

-- Here, we've chained together actions such that each action relies on the
-- preceding action's result. If at any point Pierre is too off balance, he
-- will slip and fall, and the result will be Nothing.
ex1 = landLeft 1 (0,0) >>= landRight 1 >>= landLeft 2
ex2 = landLeft 1 (0,0) >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)

-- Always slip when hitting this
banana :: Pole -> Maybe Pole
banana _ = Nothing

ex3 = landLeft 1 (0,0) >>= banana >>= landRight 1

-- We can get the banana effect without a separate function by using (>>)
nothing3 = Nothing >> Just 3
just5 = Just 3 >> Just 5
nothing4 = landLeft 1 (0,0) >>= landLeft 1 >> Nothing >>= landRight 1
-- >> will ignore the first argument, but still take into account its
-- context!  Incredible!!!

-- We can also nest the >>= operations. A nothing anywhere has the same
-- effect as above
just3Emph = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
--nothing = Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))

-- Same as above
foo :: Maybe String
foo = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

-- Syntactic sugar: the do syntax merely chains monadic bind actions as
-- above
foo' :: Maybe String
foo' = do
        x <- Just 3
        y <- Just "!"
        Just (show x ++ y)

-- Computes Just (3,2)
routine :: Maybe Pole
routine = do
        first <- landLeft 2 (0,0)
        second <- landRight 2 first
        landLeft 1 second

routineWithBanana :: Maybe Pole
routineWithBanana = do
        first <- landLeft 2 (0,0)
        Nothing -- This uses the >> operator
        second <- landRight 2 first
        landLeft 1 second

-- The List Monad
-- Recall, the list monad models non-determinism
{-
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f x)
    fail _ = []
-}

-- This produces [[3,-3], [4,-4], [5,-5]] which is then concatenated into
-- [3,-3,4,-4,5,-5]
lex1 = [3,4,5] >>= \x -> [x,-x]
--lempty = [] >>= \x -> ["bad", "mad", "rad"]

-- Non-determinism propagation
-- List comprehensions are syntactic sugar for non-deterministic propagation in
-- the list monad
lex2 = [1,2] >>= \n -> ['a', 'b'] >>= \ch -> return (n,ch)
lex2' = [(n,ch) | n <- [1,2], ch <- ['a','b']]

-- The MonadPlus Typeclass exists for Monads that also act as Monoids
{-
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
-}
-- where mzero acts like mempty and mplus works like mappend. Together with
-- the guard function, this implements filtering in list comprehensions.

-- A Knight's Quest Example
type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
        (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                   ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
        guard (c' `elem` [1..8] && r' `elem` [1..8])
        return (c',r')

in3Moves :: KnightPos -> [KnightPos]
in3Moves start = do
        first <- moveKnight start
        second <- moveKnight first
        moveKnight second

in3Moves' :: KnightPos -> [KnightPos]
in3Moves' start = moveKnight start >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3Moves' start


-- The function (<=<) composes two monadic functions, as (.) does for normal
-- functions. This relies on the 3rd monad law (associativity of >>=).
