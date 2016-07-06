{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.Bits as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

extractKey :: ByteString -> ByteString -> ByteString
extractKey oBS mBS = BS.pack $ filter (/=0) $ BS.zipWith B.xor oBS mBS

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret oFP mFP = do
    oBS <- BS.readFile oFP
    mBS <- BS.readFile mFP
    return $ extractKey oBS mBS

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey k path = do
    encStr <- BS.readFile $ path ++ ".enc"
    let decStr = BS.zipWith B.xor (BS.cycle k) encStr
    BS.writeFile path $ BS.pack decStr

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
    jsonStr <- BS.readFile path
    return $ decode jsonStr

-- Exercise 4 -----------------------------------------

filterTrxns :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
filterTrxns Nothing ts = ts
filterTrxns _ Nothing = Nothing
filterTrxns (Just tids) (Just ts) = Just $ filter isBad ts 
    where isBad (Transaction {tid = tid}) = tid `elem` tids

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs viPath tsPath = do
    vList <- parseFile viPath
    tList <- parseFile tsPath
    return $ filterTrxns vList tList

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = Map.fromListWith (+) . concat . map flatten
    where flatten (Transaction f t amt _) = [(f, -amt), (t, amt)]

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . L.maximumBy (compare `F.on` snd) . Map.toList

-- Exercise 7 -----------------------------------------

type FlowInfo = (String, Integer)

-- Helper function to separate flow into payers and payees
separate :: [FlowInfo] -> ([FlowInfo], [FlowInfo])
separate [] = ([], [])
separate l = foldr sep ([], []) l
    where sep info (payers, payees)
              | snd info > 0 = (info:payers, payees)
              | snd info < 0 = (payers, info:payees)
              | otherwise = (payers, payees)

-- Helper function to sort payers and payees in descending order
sortFlows :: ([FlowInfo], [FlowInfo]) -> ([FlowInfo], [FlowInfo])
sortFlows (l, l') = (reverse $ L.sortBy (compare `F.on` snd) l,
                     L.sortBy (compare `F.on` snd) l')

-- Helper funciton to generate new transactions to compensate loss and gain
compensate :: ([FlowInfo],[FlowInfo]) -> [TId] -> [Transaction]
compensate ([], _) _ = [] 
compensate (_, []) _ = []
compensate (_, _) [] = []
compensate (f:fs, f':fs') (i:is) = (gen f f' i):(compensate (fs, fs') is)
    where gen payer payee tid =  Transaction { from = fst payer,
                                               to = fst payee,
                                               amount = min (snd payer) ((abs . snd) payee),
                                               tid = tid }

-- Helper function to update the flow using generated transactions
update :: Transaction -> Map String Integer -> Map String Integer
update t m = updatePayee t $ updatePayer t m

updatePayer :: Transaction -> Map String Integer -> Map String Integer
updatePayer (Transaction { from = name, amount = amt }) = Map.update f name
    where f v = Just $ v - amt

updatePayee :: Transaction -> Map String Integer -> Map String Integer
updatePayee (Transaction { to = name, amount = amt }) = Map.update f name
    where f v = Just $ v + amt

-- Recover the transactions
undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids = let flows = (sortFlows . separate $ Map.toList m) in
                    case flows of
                        ([], _) -> []
                        (_, []) -> []
                        fl -> let ts = compensate fl tids in
                                  ts ++ undoTs (L.foldr update m ts) (drop (length ts) tids)
                    

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path obj = BS.writeFile path $ encode obj

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          putStrLn $ show $ (sortFlows . separate) $ Map.toList (L.foldr update flow $ compensate ((sortFlows . separate) $ Map.toList flow) (repeat ""))
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

