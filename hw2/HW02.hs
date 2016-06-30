{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = foldl (\acc p -> if fst p == snd p then acc + 1 else acc ) 0 (zip a b)

-- Exercise 2 -----------------------------------------

-- Helper function to calculate occurrence of a color
count :: (Peg -> Bool) -> Code -> Int
count p cs = length $ filter p cs

-- For each peg in xs, count how many times it occurs in ys
countColors :: Code -> [Int]
countColors cs = map (\c -> count (c ==) cs) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches s g = sum $ map (uncurry min) $ zip (countColors s) (countColors g)

-- Exercise 3 -----------------------------------------

nonExactMatches :: Code -> Code -> Int
nonExactMatches s g = matches s g - exactMatches s g

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g (exactMatches s g) (nonExactMatches s g)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move c _ _) c' = getMove c' c == m

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs = filter (isConsistent m) cs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = [ color:c | color <- colors, c <- allCodes (n-1) ]

-- Exercise 7 -----------------------------------------

-- 1. Get all possible move
-- 2. Get the first as the guess
-- 3. Use the guess to filter the list to contain only consistent ones
-- 4. Repeat from 2, using the filtered list as the input

pickGuess :: Code -> [Code] -> [Move]
pickGuess _ [] = []
pickGuess s (x:xs) = let move = getMove s x in
                         [move] ++ (pickGuess s $ filterCodes move xs)

solve :: Code -> [Move]
solve sec = pickGuess sec $ allCodes $ length sec

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
