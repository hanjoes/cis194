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

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
