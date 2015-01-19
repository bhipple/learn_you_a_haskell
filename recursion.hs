maximum' [] = error "max of empty"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' [] = error "max of empty"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
