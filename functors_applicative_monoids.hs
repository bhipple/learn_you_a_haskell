import Data.Functor
import Control.Applicative

-- From Control.Applicative, we get <$>, which is fmap as an infix operator
-- for convenient operations on Functors.

-- Being an instance of Applicative gives two functions:
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
--
-- Pure takes a value of any type and returns an applicative functor with
-- the value "inside" it.

-- We can chain IO actions together using applicative
-- These two functions are equivalent:
myAction :: IO String
myAction = do
        a <- getLine
        b <- getLine
        return $ a ++ b

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

-- We can use Applicative operations on lists to model non-deterministic
-- computations:
ex1 = [(+), (*)] <*> [1,2] <*> [3,4]
ex2 = (++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "."]
ex3 = filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]

-- Functions are applicatives.  Take a look:
-- Here, the 5 first gets applied to the +3 and *100, resulting in 8 and
-- 500. Then the (+) operates on these two values and gives us 508.
fiveHundredEight = (+) <$> (+3) <*> (*100) $ 5
just8 = (+) <$> Just 3 <*> Just 5

-- If we want applicative operations on lists to model zipping instead of
-- non-deterministic combination, we can make it an instance of the ZipList
-- type.
-- (We have to use getZipList because ZipList is not an instance of show)
ex4 = getZipList $ max <$> ZipList [1,2,3,4,5] <*> ZipList [5,4,3,2,1]
ex5 = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"

-- Applicative functions are subject to the following laws:
{-
pure f <*> x = fmap f x
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
-}

-- The Newtype Keyword
-- Using this lets us know we're just wrapping another type. It's like the
-- data keyword, but it's restricted to one value constructor with one
-- field. This trades flexibility for runtime performance optimizations.
{-
    newtype ZipList a = ZipList { getZipList :: [a] }
-}

-- Keyword refresher:
-- type => just creates a synonym with no effect other than readability
-- newtype => take a type and wrap it in a new type, separate from the original,
--   so that we can give it new typeclasses
-- data => creates a new type

-- A Monoid is an associative binary operator whose output type is the same as its input
-- type, together with an identity, following these laws:
{-
  mappend mempty x = x
  mappend x mempty = x
  (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-}
-- i.e., identity and associativity. Note that we do not require
-- commutativity!
--
-- Data.Monoid exports two familiar monoids: Product and Sum

main = putStrLn "http://learnyouahaskell.com/functors-applicative-functors-and-monoids"
