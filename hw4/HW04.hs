{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

trimTail :: (Eq a, Num a) => [a] -> [a]
trimTail = reverse . trimHead . reverse

trimHead :: (Eq a, Num a) => [a] -> [a]
trimHead = dropWhile (==0)

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P l) (P l') = (trimTail l) == (trimTail l')

-- Exercise 3 -----------------------------------------


-- Get coefficient given the number
getce :: (Num a, Eq a, Show a) => a -> String
getce 1 = ""
getce n = show n

-- Generate pairs for array element with its index

items :: [a] -> [(a, Integer)]
items l = zip l [0,1..]

-- Get the term given a pair of coefficient and index
getTerm :: (Num a, Eq a, Show a) => (a, Integer) -> String
getTerm (0, _) = "0"
getTerm (n, 0) = show n
getTerm (n, 1) = getce n ++ "x"
getTerm (n, i) = getce n ++ "x^" ++ show i

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P l) = case trimTail l of
        [] -> "0"
        xs -> let terms = map getTerm $ items xs in
                 intercalate " + " $ reverse $ filter (/="0") $ terms
      

-- Exercise 4 -----------------------------------------

concatPoly :: Poly a -> Poly a -> Poly a
concatPoly (P l) (P l') = P (l ++ l')

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) (P l) = P l
plus (P l) (P []) = P l
plus (P (n:ns)) (P (n':ns')) = concatPoly (P [n + n']) (P ns `plus` P ns')

-- Exercise 5 -----------------------------------------

getComb :: Num a => Poly a -> Poly a -> [Poly a]
getComb (P []) (P _) = []
getComb (P _) (P []) = []
getComb (P (n:ns)) (P l) = (P $ map (*n) l):(getComb (P ns) (P $ 0:l))

times :: Num a => Poly a -> Poly a -> Poly a
times p p' = sum $ getComb p p'

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P l) = P $ map negate l
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P l) n  = evalP $ items l
    where evalP ((ce, i):ps') = n ^ i * ce + evalP ps'
          evalP [] = 0

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 f = f
    nderiv n f = nderiv (n-1) (deriv f) 

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P l) = P $ drop 1 $ calcTerms $ items l
        where calcTerms [] = []
              calcTerms ((n, i):ps) = (n * (fromInteger i)):(calcTerms ps)

