module Lib where
-- module Lib (someFunc) where

import Data.List
import System.IO

-- comment
{-
Multiline comment
-}


someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    putStrLn "Whats your name"
    name <- getLine
    putStrLn("Hello " ++ name)

maxInt = maxBound :: Int

sumOfNums = sum [1..1000]

addEx = 5 + 4
modEx = mod 5 4
modEx2 = 5 `mod` 4
negNumEx = 5 + (-4)

num8 = 8 :: Int
num9 :: Int
num9 = 9
sqrtOf9 = sqrt (fromIntegral num9)

floorVal = floor 9.99
ceilingVal = ceiling(9.99)
truncateVal = truncate 9.99
roundVal = round 9.99 

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not True

-- LISTS--

primeNumbers = [3, 5, 7, 11]
morePrimes = primeNumbers ++ [13, 17, 19]
favNums = 2: 7:9:[]
multList = [[3, 5, 7], [7, 9, 11]]
morePrimes2 = 2 : morePrimes
lenPrime = length morePrimes
reversePrime = reverse morePrimes2
isListEmpty = null morePrimes2
secondPrime = morePrimes2 !! 1
firstPrime = head morePrimes2
lastPrime = last morePrimes2
primeInit = init morePrimes2 -- everything but last value
first3Primes = take 3 morePrimes2
restOfPrimes = drop 3 morePrimes2
is7InList = 7 `elem` morePrimes2
maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2
sumPrimes = sum morePrimes2
newList = [2, 3, 5]
productPrimes = product newList
zeroToTen = [0..10]
evenList = [2, 4 .. 20]
letterList = ['A', 'C'  .. 'Z']
shouldBeTrue = ['a', 'b'] == "ab"

-- Haskell is lazy, will only populate when it needs to
infinPow10 = take 50 [10, 20 ..] 
many2s = take 10 (repeat 2)
many3s = replicate 10 3
cycleList = take 10 (cycle [1,2,3,4])
listTimes3Filtered = [x * 3 | x <- [1 .. 10], x *3 <= 20]
divisibleBy9And13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]
sortedList = sort [9, 34, 5, 16, 8]
sumOfLists = zipWith (+) [1, 2, 3] [4, 5, 6]
listBiggerThan5 = filter (>5) morePrimes
listBiggerThan7 = [ x | x <- morePrimes, x > 7]
evensUpto20 = takeWhile (<=20) [2, 4 .. ]

-- apply operation to each item on list
-- foldl from left to right
-- foldr from right to left 
multOfList = foldl (*) 4 [2,3,5] -- gives 4 * 2 * 3 * 5

pow3List = [3^n | n <- [1..10]]
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

-- TUPLES --

randTuple = (1, "Random Tuple")
bobSmith = ("Bob Smith", 52)
bobsName = fst bobSmith
bobsAge = snd bobSmith
names = ["Bob", "Tom"]
addresses = ["123 Main", "234 Secondary"]
namesNAdresses = zip names addresses

{-
Only list of same type allowed
But in a tuple, you can throw everything in

Tuple can only be made once. Cannot edit or remove items later on. Like you can in list.
-}

-- FUNCTIONS --
num7 = 7
getTriple x = x  * 3
tripled7 = getTriple num7

addMe :: Int -> Int -> Int
addMe x y = x + y

-- this will work with floats as well
-- implicit type annotation - sumMe :: Num a => a -> a -> a
sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1+x2, y1+y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 21 = "Hurray you are an adult"
whatAge x = "Nothing important"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

prodFact n = product [1..n]

-- guards
isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True

isEven n = n `mod` 2 == 0

whatGrade :: Int -> String
whatGrade age
    | (age >= 5) && (age <= 6) = "Kindergarten"
    | (age >= 7) && (age <= 18) = "School"
    | (age >= 19) && (age <= 24) = "University"
    | otherwise = "Go to work"

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
    | avg <= 0.200 = "Terrible"
    | avg <= 0.250 = "Average"
    | avg <= 0.300 = "Good"
    | otherwise = "Superstar"
    where avg = hits/atBats

getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st list item is " ++ show x ++ " and rest are " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty string"
getFirstItem all@(x:_xs) = "The first letter in " ++ all ++ " is " ++ [x]

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1, 2, 3]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

areStringsEqual :: [Char] -> [Char] -> Bool
areStringsEqual [] [] = True
areStringsEqual (x:xs) (y:ys) = x == y && areStringsEqual xs ys
areStringsEqual _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3

getAddFunc :: Int -> (Int -> Int) -- same as Int -> Int -> Int
getAddFunc x y = x + y
adds3 = getAddFunc 3
fourPlus3 = adds3 4
threePlusList = map adds3 [1, 4, 5]

-- lambdas
dbl1To10 =  map (\x ->  x * 2) [1..10]

-- if statements, not used as much
doubleEvenNumber y =
    if y `mod` 2 /= 0
        then y
        else y*2

getClass :: Int -> String
getClass n = 
    case n of
        5 -> "Go to Kindergarten"
        6 -> "Go to Elementary"
        _ -> "Go away"

-- ENUMERATIONS

data CricketPlayer = Batsman 
                    | Bowler
                    | AllRounder
                deriving Show

viratKohli :: CricketPlayer -> Bool
viratKohli Batsman = True
viratIsBatsman = print (viratKohli Batsman)

-- CUSTOM TYPES

data Customer = Customer String String Double
    deriving Show

tomSmith = Customer "Tom Smith" "123 Addess" 9.87

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b

data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper beats Rock"
shoot Paper Scissors = "Scissors beats rocks"
shoot Rock Scissors = "Rock beats Scissors"
shoot _ _ = "Error"


data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving Show

area :: Shape -> Float
area (Circle _ _ radius) = pi * radius ^ 2
area (Rectangle _ _ l w) = l * w

-- data: zero or more constructors, each can contain zero or more values.
-- DataFoo and Int totally different.
data DataFoo = DataFoo Int

-- newtype: similar to data but exactly one constructor and one value in that constructor,
-- and has the exact same runtime representation as the value that it stores.
-- Foo and Int different at compile time but same at runtime.
newtype NewTypeFoo = NewTypeFoo Int

-- type: type synonym, compiler more or less forgets about it once it is expanded.
-- Foo and Int same at compile time and runtime.
type Foo = Int

-- $ sign to get rids of parantheses
area1 = area (Circle 0 0 5)
area2 = area $ Circle 0 0 5

sumValue = putStrLn $ show $ 1+2
sumValue2 = putStrLn . show $ 1+2


-- Type classes
-- Num, Eq, Ord, Show
-- corresponds to sets of types which have certain operation defined for them

data Employee = Employee {name :: String, position :: String, idNum :: Int}
    deriving (Show, Eq)

samSmith = Employee {name="Sam Smith", position = "Manager", idNum =1 }
pamMarx = Employee {name="Pam Marx", position = "Sales", idNum = 2 }

isSamPam = samSmith == pamMarx
samSmithData = show samSmith

data ShirtSize = S | M | L
-- implement Eq class
instance Eq ShirtSize where
    S == S = True
    M == M = True
    L == L = True
    _ == _ = False

-- implement Show Class
instance Show ShirtSize where
    show S = "Small"
    show M = "Medium"
    show L = "Large"

smallAvail = S `elem` [S, M, S, L]

theSize = show S


-- Custom type classes

--declare class
class MyEq a where
    areEqual :: a -> a -> Bool

--implement class
instance MyEq ShirtSize where
    areEqual S S = True
    areEqual M M = True
    areEqual L L = True
    areEqual _ _ = False
 
newSize = areEqual M M

-- IO --

sayHello = do
    putStrLn "Whats your name"
    name <- getLine
    putStrLn $ "Hello " ++ name

writeToFile = do
    theFile <- openFile "test.txt" WriteMode
    hPutStrLn theFile "Random Line of text"
    hClose theFile

readFromFile = do
    theFile2 <- openFile "test.txt" ReadMode
    contents <- hGetContents theFile2
    putStr contents
    hClose theFile2

------------------------------------------------
-- Functors Applicatives and Monads in Haskell--
-------------------------------------------------

----- FUNCTOR -----

-- a Functor is a typeclass
-- a typeclass is kindof like an interface, gives us a signature for a method and we
-- we have to implement it ourselves

-- Functor typeclass Already included with Haskell
{-
class Functor f where
    fmap :: (a -> b) ->  f a -> f b
-}
-- In haskell, we have wrapped values and unwrapped values
-- eg. unwrapped value- add 4 5, both 4 and 5 are unwrapped values
-- e.g. wrapped value - Just 5, here 5 is wrapped around the Maybe type

-- cant apply (+3) to a maybe value but can to 5
-- thats where Functors come into play
 
-- so we want to be able to do:
-- fmap (+3) (Just 4) -> Just 7

data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap _func Nothing2 = Nothing2

-- Functor is already defined for a lot of types like Maybe, List.
-- for list fmap = map
 
-- So a Functor applies function to a wrapped value and gives wrapped value back


-- infix notation
-- identical as fmap
-- (+3) <$> (Just2 4)

-- e.g. of defining Functor for custom data type
-- in this case apply function to each tip

data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show
instance Functor Tree where
    fmap func (Tip a) = Tip (func a)
    fmap func (Branch left right) = Branch (fmap func left) (fmap func right)

exampleTree = Branch (Tip 4) (Branch (Tip 5) (Tip 6))
exampleTreePlus3 = (+3) <$> exampleTree


------- APPLICATIVE --------

-- also a type class
-- also already defined
{-
class Functor f => Applicative f where
    pure :: a -> f a
    <*> :: f (a-> b) -> f a -> f b
-}

-- this is the infix notation, doesnt have a normal function notation like fmap did

shouldGetJust11 = fmap (+3) (Just 8)
--applicatives take this one step further
-- allow us to wrap functions
shouldGetMaybe9 = Just (+1) <*> (Just 8)
shouldGetNothing = Nothing <*> (Just 8)

-- our own datatype

instance Applicative Maybe2 where
    -- pure is usually the "simplest version" of our type
    pure a = Just2 a
    -- could do this way : 
    -- Just2 f <*> Just2 a = Just2 (f a)
    -- but already have defined Functor earlier so can do this way:
    Just2 func <*> a = func <$> a
    Nothing2 <*> _ = Nothing2

shouldGetNothing2 = Nothing2 <*> (Just2 8)
shouldGetNothing2Again = Just2 (+3) <*> Nothing2
shouldGetMaybe2Val = Just2 (+3) <*> Just2 1

-- the left 
shouldGetAMaybeMultiplier ::  Maybe2 (Integer -> Integer)
shouldGetAMaybeMultiplier = (*) <$> (Just2 8)

shouldGetJust2_16 = shouldGetAMaybeMultiplier <*> Just2 2

appliedAListOfFunctionsToList = [(*2), (*3)] <*> [1, 2, 3]

-- defining for tree
-- we want to be able to transform a Tree by a Tip containing a function for e.g.  

instance Applicative Tree where
    pure a = Tip a
    Tip func <*> tree = fmap func tree
    Branch left right <*> tree = Branch (left <*> tree) (right <*> tree) 


exampleTreeMultipliedBy4 = Tip (*4) <*> exampleTree
-- this gives 2 a tree with 2 branches,
-- one where exampleTree multiplied by 4 and one where its multiplied by 2
exampleTreeMultipliedByLeft4Right2 = Branch (Tip (*4)) (Tip (*2)) <*> exampleTree

---------- MONADS -------------

-- also a typeclass
-- applies a regular function (unwrapped) to a wrapped value and returns a wrapped value
-- isnt this what a functor does?
-- kind of right, but some difference

half x  = if even x
    then Just (x `div` 2)
    else Nothing

shouldGetJust1 = half 2
-- but half (Just 2) doesnt work because function doesnt accept wrapped values
-- fmap half (Just 2) returns a doubly wrapped value Just (Just 1)

 -- this is monad bind, basically a
-- streaming operator, takes in the left part and
-- places it on right side of functiona
shouldGetJust2 = Just 4 >>= half

-- can chain like this
shouldGetJust8 = Just 32 >>= half >>= half

{-
class Monad m where 
    -- bind
    >>= :: m a -> (a -> m b) -> m b
    -- "m" is the monad like Maybe2
    -- "m a" is like "Just2 4"
    -- (a -> m b) takes in a regular value and returns a monad (wrapped value)
    -- eg. half :: Integer -> Maybe Integer
    -- returns the monad
-}

instance Monad Maybe2 where
    Nothing2 >>= func = Nothing2
    Just2 a >>= func = func a

half2 x  = if even x
    then Just2 (x `div` 2)
    else Nothing2

shouldGetJust2_8 = Just2 32 >>= half2 >>= half2

-- Applicatives require Functors to be defined
-- Monads require both Applicatives and Functors to be defined

-- for our custom Tree data structue

instance Monad Tree where
    Tip a >>= func = func a
    Branch left right >>= func = Branch (left >>= func) (right >>= func)


g x | x == 4 = (Tip 99) | otherwise = Branch (Tip (x * 2)) (Tip (x * 4))
examplegAppliedToTree = exampleTree >>= g
