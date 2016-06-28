{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n
  | n < 0 = []
  | otherwise = [lastDigit n] ++ (toRevDigits $ dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [a] = [a]
doubleEveryOther [a,b] = [a, 2*b]
doubleEveryOther (a:b:xs) = [a,2*b] ++ (doubleEveryOther xs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum $ toRevDigits x
sumDigits (x:xs) = sumDigits [x] + sumDigits xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

-- This function takes an integer of num of levels and three pegs, the first
-- is the starting peg and the second is the target peg
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = move (n - 1) a c b ++ moveTop a b ++ move (n - 1) c b a

-- This function moves n levels from the first to the second peg.
move :: Integer -> Peg -> Peg -> Peg -> [Move]
move 0 _ _ _ = []
move 1 s t _ = [(s, t)]
move n s t c = hanoi n s t c

-- This funciton simply moves the first level of the tower.
moveTop :: Peg -> Peg -> [Move]
moveTop a b = [(a, b)]
