-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testRevDigit :: (Integer, [Integer]) -> Bool
testRevDigit (n, ds) = toRevDigits n == ds

ex2Tests :: [Test]
ex2Tests = [Test "toRevDigits test" testRevDigit
            [(1234, [4,3,2,1]), (0, []), (-10, [])]]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (xs1, xs2) = doubleEveryOther xs1 == xs2

ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther test" testDoubleEveryOther
            [([1,2,3,4], [1,4,3,8]), ([0,1], [0,2]), ([1,0], [1,0]), ([],[])]]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (as, a) = sumDigits as == a

ex4Tests :: [Test]
ex4Tests = [Test "sumDigits test" testSumDigits
            [([], 0), ([10,2,4,18], 16), ([0, 0, 0], 0)]]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (a, b) = luhn a == b

ex5Tests :: [Test]
ex5Tests = [Test "luhn test" testLuhn
            [
	    (5594589764218858, True),
	    (1234567898765432, False),
	    (378215049125709, True),
	    (343070311111578, True),
	    (349865302208216, True),
	    (349279262086457, True),
	    (343427597458562, True),
	    (344148842699862, True),
	    (376983327086412, True),
	    (349927306342233, True),
	    (343657689943390, True)
	    ]]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
