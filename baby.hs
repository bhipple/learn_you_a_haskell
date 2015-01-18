doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

a = [x*y | x <- [2,5,10], y <- [8, 10, 11]]

removesUpperCase str = [c | c <- str, c `elem` ['a'..'z']]

-- Alternate ways of doing factorial:
factorial 0 = 1
factorial x = x * factorial (x-1)
fact x = product [1..x]

length' [] = 0
length' (_:xs) = 1 + length' xs

-- Let binding can go into the function like this
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- Case expressions
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               _:[] -> "a single element"
                                               x:y:_ -> "long"
