doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else doubleMe x

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

length' xs = sum [ 1 | _ <- xs ]

removeLowercase st = [char | char <- st, char `elem` ['A'..'Z']]

-- rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b]]

-- define the type of the function
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

-- type of multiple parameters to the function
addThree :: Int -> Int -> Int -> Int
addThree z y x = z + y + x

-- Pattern matching of functions
lucky :: (Integral a) => a -> String
lucky 7 = "GOT 7!!"
lucky x = "Stiff shit"

-- Recursion example, terminates because we hit "factorial 0"
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial(x-1)

-- Add two vectors - simple
addSimpleVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addSimpleVectors a b = (fst a + fst b, snd a + snd b)
-- Add two vectors, better use of pattern matching 
addSimpleVectors2 :: (Num a) => (a,a) -> (a,a) -> (a,a)
addSimpleVectors2 (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

-- function to get the third element out of a tuple, not using the first
-- or second parts, so use a _. 
third :: (a,b,c) -> c
third (_,_,c) = c

-- Our own head fucntion my list mattern matching
head' :: [a] -> a
head' []    = error "empty!"
head' (x:_) = x

-- advanced pattern matching on a function
tell :: (Show a) => [a] -> String
tell []       = "Empty"
tell (x:[])   = "Array has one element" ++ show x
tell (x:y:[]) = "Two elements: " ++ show x ++ " " ++ show y
tell (x:y:_)  = "More than two elements, second one is" ++ show y

-- length function using recursion
length2 :: (Num b) => [a] -> b
length2 []    = 0
length2 (_:x) = 1 + length2 x

-- sum function with recursion
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 'as patterns' - parse a lists elements but get access to the whole thing
capital :: String -> String
capital "" = "Empty!"
capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]


-- guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Underweirgh"
    | bmi <= normal = "Average"
    | bmi <= fat = "Fat"
    | otherwise = "Obese"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' x y
    | x > y = x
    | otherwise = y

-- Implement our own comparison, define function in infix notation if we want
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

-- pattern matching in guards
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- define functions in where blocks
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2 

-- let functions are like where functions but don't span across guards,
-- they are very local
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in  sideArea + 2 * topArea

-- Rewrite calcBmis to use let
calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2 ]

-- 







