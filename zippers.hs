main = putStrLn "http://learnyouahaskell.com/zippers"

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Direction = L | R deriving (Show)
type Directions = [Direction]

-- Given a list of L|R directions, find the element in the tree at that
-- location relative to the root.
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r
