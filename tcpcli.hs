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

data Settings = Settings
  { path       :: String
  , eps        :: Int }

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

main :: IO ()
main = sendFile =<< execParser opts
  where
    opts = info (settings <**> helper)
      ( fullDesc
     <> progDesc "Send strings delimited by '\\n' from file to socket with some EPS")

sendFile :: Settings -> IO ()
sendFile (Settings path eps) = do events <- readLines path
                                  runTCPClient "127.0.0.1" "13666" (handleSocket eps events)

handleSocket :: Int -> [String] -> Socket -> IO () 
handleSocket eps events socket = do
                                     start <- getCPUTime
                                     let (msg, rest) = takeAtMostN eps events
                                     if msg == [] then return () else do
                                     let package = concatMap (\s -> if s == [] then "" else s ++ "\n") msg
                                     sendAll socket (C.pack package)
                                     end <- getCPUTime
                                     let elapsed = end - start
                                     if elapsed < 10^12 then do
                                         threadDelay ((fromInteger (10^12 - elapsed)) `div` 10^6)
                                         handleSocket eps rest socket
                                     else handleSocket eps rest socket
                                     
                                 
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

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

takeAtMostN :: Int -> [String] -> ([String], [String])
takeAtMostN n strs = if length strs <= n then (strs, []) else (take n strs, drop n strs)

