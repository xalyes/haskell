{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Options.Applicative
import Data.Semigroup ((<>))

import System.CPUTime
import Control.Concurrent

import Data.List
import Data.Foldable
import Data.List.Split

import Text.Printf

data Settings = Settings
  { path       :: String
  , eps        :: Int 
  , threads    :: Int } 

settings :: Parser Settings
settings = Settings
      <$> strOption
          ( long "path"
         <> short 'p'
         <> metavar "FILE"
         <> help "File to send" )
      <*> option auto
          ( long "eps"
         <> short 'e'
         <> help "How many Events Per Seconds send"
         <> metavar "NUM" )
      <*> option auto
          ( long "threads"
         <> short 't'
         <> help "How many thread use"
         <> metavar "NUM" )

main :: IO ()
main = sendFile =<< execParser opts
  where
    opts = info (settings <**> helper)
      ( fullDesc
     <> progDesc "Send strings delimited by '\\n' from file to socket with some EPS")

sendFile :: Settings -> IO ()
sendFile (Settings path eps threads) = do  events <- C.readFile path
                                           --let chnks = processData eps events
                                           --let chList = nChunks threads chunks
                                           mVars <- sequence [newEmptyMVar | i <- [1..threads]]
                                           --let threadData = zip mVars chList
                                           start <- getCPUTime
                                           sequence_ [forkIO $ do runTCPClient "127.0.0.1" "13666" (handleSocket [events]); putMVar mVar 1 | mVar <- mVars]
                                           rs <- sequence [takeMVar mVar | mVar <- mVars]
                                           end <- getCPUTime
                                           let diff = (fromIntegral (end - start)) / (10^12)
                                           printf "Sending time: %0.3f sec\n" (diff :: Double)
                                           let resultEps = (fromIntegral $ (C.length $ C.filter (\e -> e == '\n') events) * threads) / diff
                                           printf "EPS: %0.3f sec\n" (resultEps :: Double)

handleSocket :: [C.ByteString] -> Socket -> IO () 
handleSocket (msg:rest) socket = do
                                         --start <- getCPUTime
                                         -- let package = concatMap (\s -> if s == [] then "" else s ++ "\n") msg
                                         sendAll socket (msg)
                                         --end <- getCPUTime
                                         if rest == [] then return () else do
                                         --let elapsed = end - start
                                         --if elapsed < 10^12 then do
                                           --  threadDelay ((fromInteger (10^12 - elapsed)) `div` 10^6)
                                            -- handleSocket rest socket
                                         handleSocket rest socket
                                     
                                 
    -- msg <- recv s 1024
    -- putStr "Received: "
    -- C.putStrLn msg

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

nChunks n [] = []
nChunks n l = let len = length l `div` n
             in (take len l : nChunks (n-1) (drop len l))

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

splitToChunks [] s offset = [s]
splitToChunks (i:[]) s offset = let (chunk, rest) = Data.List.splitAt (i-offset) s in [chunk, rest]
splitToChunks (i:idxs) s offset = let (chunk, rest) = Data.List.splitAt (i-offset) s
                                  in chunk : splitToChunks idxs rest i

everyN = \n -> map head . takeWhile (not . Prelude.null) . iterate (Prelude.drop n)

getLineIndexes = \s -> Data.List.findIndices (\e -> e == '\n') s

processData eps events = let seqEvents = events
                         in splitToChunks (map (\i -> i-1) $ Prelude.drop 1 $ everyN eps $ getLineIndexes seqEvents) seqEvents 0

