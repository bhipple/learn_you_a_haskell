import Data.List

-- Reverse polish notation calculator
-- Look how incredibly easy this is to implement with the magic of
-- functional programming!
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunc [] . words
    where foldingFunc (x:y:ys) "*" = (x*y):ys
          foldingFunc (x:y:ys) "+" = (x+y):ys
          foldingFunc (x:y:ys) "-" = (y-x):ys
          foldingFunc (x:y:ys) "/" = (y/x):ys
          foldingFunc (x:y:ys) "^" = (x**y):ys
          foldingFunc (x:ys) "ln" = log x : ys
          foldingFunc xs "sum" = [sum xs]
          foldingFunc xs numStr = read numStr : xs

testRPN :: String -> IO ()
testRPN e = putStrLn $ e ++ " ==> " ++ show (solveRPN e)


main = do
        putStrLn "Reverse polish notation calculator:"
        let examples = [
                   "10 4 3 + 2 * -",
                   "2.7 ln",
                   "10 ^ 2",
                   "10 10 10 10 sum 4 /",
                   "6.25 0.5 ^"
                   ]
        mapM_ testRPN examples
