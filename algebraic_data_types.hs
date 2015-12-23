
-- We define a new type like so:
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- This is a function like anything else, which takes 3 or 4 parameters and
-- return a shape. Take a look with
-- :t Circle
-- :t Rectangle
-- By deriving show, GHCi can print a constructed Shape (default just uses
-- the type constuctor arguments)

-- This function takes a Shape and calculates the area of the shape.
-- We add conditional context to the pattern matching to make it dispatch
-- to the appropriate implementation based on the parameter.
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x1 - x2) * (abs $ y1 - y2)

-- Since constructors are functions, we can partially apply them.
nConcentricCircles :: Float -> Float -> [Float] -> [Shape]
nConcentricCircles x y rs = map (Circle x y) rs

-- It's types all the way down
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point

-- Pattern matching lets us pull the fields out of the data type.
-- We can even use nested pattern matching!
surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)


-- Record Syntax:
-- We can also lookup fields off algebraic data types by name:
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- This does a few things:
-- 1) It makes "Show" display the constructed Person as key-value pairs
-- 2) Constructor calls no longer have to specify the fields in order
-- 3) For every key, it creates a function of that name that maps a Person
-- to that field.  For example, the compiler will give us:
-- phoneNumber :: Person -> String

-- Per convention, *never add typeclass constraints in data declarations*.
-- If we do, then every function that uses the type requires the contraint
-- to compile, even if it wouldn't have otherwise
-- E.g., if we do:
-- data (Ord k) => Map k v = ...
-- Then every function taking a map will require the (Ord k) constraint,
-- even if it doesn't care, such as the toList function.

data Vector a = Vector a a a deriving (Show)

vPlus :: (Num t) => Vector t -> Vector t -> Vector t
vPlus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

vMult :: (Num t) => Vector t -> t -> Vector t
vMult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

scalarProduct :: (Num t) => Vector t -> Vector t -> t
scalarProduct (Vector i j k) (Vector l m n) = i*l + j*m + k*n


-- When deriving Ord with multiple constructors, the values are considered
-- to be increasing
data Demo = Ben Int | Inna Int deriving (Show, Eq, Ord)

-- First we compare the constructor used; if that's equal, we compare the
-- elements inside
thisIsTrue = (Ben 3) < (Inna 2)
thisIsFalse = (Ben 3) > (Inna 2)
thisIsTrue' = (Ben 3) < (Ben 5)

-- An example where this comes up is in the maybe type, which is defined
-- as:
data Maybe' a = Nothing | Just a
-- so nothing is always less than Just a
--

-- Some cool stuff with enums and bounded data types
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

tues = succ Monday
wed = pred Thursday
mon = minBound :: Day
week = [minBound .. maxBound] :: [Day]

-- We can also make recursive data types
data List' a = Empty | Cons a (List' a) deriving (Show,Read,Eq,Ord)
demoList1 = Empty
demoList2 = Cons 5 (Empty)
demoList3 = Cons 2 (Cons 3 (Cons 4 Empty))

-- Implementation of a binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- Note that we have to make a new subtree each time we
-- decide to go left or right. This may seem hideously inefficient, but
-- laziness takes care of some of the worst off it ...
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- How cool is this?!?!?
nums = [2,6,4,3,1,3,6,3,6,7,2,4]
numsTree = foldr treeInsert EmptyTree nums

-- Typeclassses:
-- (Added the - suffix to avoid name clashes)
-- By writing the implementations with mutual recursion,
-- we only need to provide one or the other when creating instances of this
-- typeclass
class Eq' a where
    (==-) :: a -> a -> Bool
    (/=-) :: a -> a -> Bool
    x ==- y = not (x /=- y)
    x /=- y = not (x ==- y)

data TrafficLight = Red | Yellow | Green

-- When we declare a data type as an instance of a typeclass, we have to
-- provide implementations for the functions in that typeclass
instance Eq' TrafficLight where
    Red ==- Red = True
    Yellow ==- Yellow = True
    Green ==- Green = True
    _ ==- _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- Getting the terminology straight:
-- This is a class declaration, or a typeclass
class YesNo a where
    yesno :: a -> Bool

-- The Functor typeclass requires that the instance implement fmap, which
-- maps a function over the range of elements. For the list type,
-- fmap = map

-- By using the :k command in GHCI, we can inspect type kinds
-- This is a concrete type
-- :k Int
-- Int :: *
--
-- And this is a type constructor. It takes one type as a paramter and
-- returns a concrete type!
-- :k Maybe
-- Maybe :: * -> *
--
-- :k Maybe Int
-- Maybe Int :: *
--
-- This one takes two concrete types and returns a concrete type
-- :k Either
-- Either :: * -> * -> *
