{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs 

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f s = Cons s $ sIterate f $ f s

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) s = Cons x $ sInterleave s xs

sTake :: Int -> Stream a -> [a]
sTake n (Cons x xs)
    | n <= 0 = []
    | otherwise = x:sTake (n-1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

-- In acc, (sRepeat 0) is just some dummy value
-- since foldr will never use them for infinite list
ruler :: Stream Integer
ruler = foldr step acc [0..]
    where step a b = sInterleave (sRepeat a) b
          acc = sInterleave (sRepeat 0) $ sRepeat 0

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand s = sIterate (\x -> (1103515245 * x + 12345) `mod` 2147483648) s

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 236 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs = foldl' step (Just (maxBound, minBound)) xs
    where step (Just (!mi, !ma)) n = Just (min mi n, max ma n)
          step _ _ = Nothing

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
