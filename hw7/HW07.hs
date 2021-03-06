{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Function
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM = fmap

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i1 i2 v = liftM2 swapV' (v !? i1) (v !? i2)
    where swapV' e1 e2 = v // [(i1, e2),(i2, e1)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts li v = mapM f li
    where f i = v !? i

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = let len = length v in
    case len of
        0 -> return Nothing
        _ -> do i <- getRandomR (0, len)
                return $ v !? i

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM V.fromList $ replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n r = liftM V.fromList $ replicateM n $ getRandomR r

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = do
    let range = reverse [1..(length v - 1)]
    l <- mapM shuffleI range
    foldM f v l
    where shuffleI i = do
              j <- getRandomR (0, i)
              return (i, j)
          f acc (i, j) = case swapV i j acc of
              Just newVec -> return newVec
              Nothing -> return acc

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = let vs = V.splitAt i v 
                      v1 = fst vs
                      v2 = V.tail $ snd vs
                      p = v ! i in
                      (on (V.++) (V.filter (<p)) v1 v2,
                       p,
                       on (V.++) (V.filter (>=p)) v1 v2)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v = case v !? 0 of
    Nothing -> v
    Just p -> let xs = V.tail v in
                  qsort [ y | y <- xs, y < p ]
                  <> (cons p $ qsort [ y | y <- xs, y >= p ])

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v = case length v of
    0 -> return V.empty
    _ -> do
        i <- getRandomR (0, length v - 1)
        let (p1, p, p2) = partitionAt v i
        ps1 <- qsortR p1
        ps2 <- qsortR p2
        return $ ps1 <> cons p ps2

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select r v
    | r > (length v - 1) || r < 0 = return Nothing
    | otherwise = do
          i <- getRandomR (0, length v - 1)
          let (p1, p, p2) = partitionAt v i
          let len = length p1
          case r `compare` len of
              EQ -> return $ Just p
              LT -> select r p1
              GT -> select (r - len - 1) p2
              

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = do
    s <- suits
    l <- labels
    return $ Card l s

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard dk = do
    let len = length dk
    guard $ len > 0
    return $ (V.head dk, V.tail dk)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 dk = Just ([], dk)
getCards n dk = do
    (c, dk') <- nextCard dk
    (cs, dk'') <- getCards (n - 1) $ dk' 
    return $ (c:cs, dk'')
    

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
