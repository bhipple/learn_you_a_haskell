-- Curried functions
multiThree x y z = x * y * z
multiTwoWithFive = multiThree 5
multiBy10 = multiTwoWithFive 2

-- Currying with an infix function
isUpperAlpha = (`elem` ['A'..'Z'])

-- Functions are arguments to a function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Takes a function that maps an a and b to a c as its first argument,
-- a list of a's as its second argument,
-- a list of b's as its third argument,
-- and returns a list of c's
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- e.g., zipWith' max [6, 3, 2, 1] [7, 3, 1, 5]

-- Returns function that flips arguments
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y
-- Example:
-- let g = flip' zip
-- g [1..5] "hello"

-- map: takes a function and list, returns list of f(x)'s
-- map (+3) [1,5,3,1,6]
-- map (replicate 3) [3..6]

-- filter
-- Takes a predicate and a list; returns only elements of the list for which predicate evalutes true
allDivisbleBy3829 x = filter pred [1..x]
    where pred a = a `mod` 3829 == 0

largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

-- Cool!
-- sumOfOddSquaresLessThan10k = sum (takeWhile (<10000) (filter odd (map (^2) [1..[)))

-- Collatz Conjecture
collatzChain 1 = [1]
collatzChain n
    | even n = n:collatzChain (n `div` 2)
    | odd n = n:collatzChain (3*n + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatzChain [1..100]))
    where isLong xs = length xs > 15

-- Create a function that takes a list argument and sums it up, using a left fold
sum' = foldl (+) 0

-- Use folds to check if y is in ys
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- Note that the acc x locations are swapped on right folds!
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- We could use a left fold for the map function, but since
-- ++ is much more expensive than : it makes sense to come from
-- the right and prefix the results onto the acc list

-- As a fun note on currying and partial application,
-- these two definitions are equivalent:
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

-- When dealing with infinite lists, foldr works while foldl does not.
-- Intuitively, since foldr starts at some point to the right and folds
-- leftwards, it will eventually hit the beginning of the list and
-- terminate; since foldl starts at the beginning and moves rightwards, it
-- will never reach the end and results in a Bottom computation.
-- In practice, they all start from the left, but the associativity is
-- different. From Hoogle:
--
-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
--
-- Infinite list of booleans:
isEven = map even [1..]

-- Happily terminates as soon as it finds one even
hasAnEven = foldr (||) False isEven

-- Bottom!
-- hasAnEven = foldl (||) False isEven

-- We can also use foldl1 and foldr1. These behave exactly as foldl and
-- foldr, except instead of taking an explicit accumulator starting value
-- they use the first element of the list. Both of these return 55:
sumA = foldr (+) 0 [1..10]
sumB = foldr1 (+) [1..10]
-- Note that this variety does not work on the empty list, while foldr does!

-- When building up an intuition for fold ordering, we can use scanl and
-- scanr, which report all intermediate values of the accumulator
sumScanr = scanr (+) 0 [1..10]
sumScanl = scanl (+) 0 [1..10]

-- How many elements does it take for the sum of the square roots of
-- natural numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl (+) 0 (map sqrt [1..])))

-- Aside from doing right-associative application, the $ operator can be
-- used to map function application over a list. This takes whatever
-- function happens to be in the list, and applies 3 to it
dollarMap :: [Double]
dollarMap = map ($ 3) [(+4), (*5), sqrt]

applyThree :: Num a => [a -> b] -> [b]
applyThree = map ($3)
