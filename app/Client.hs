{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception qualified as E
import Data.ByteString.Char8 qualified as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Poker qualified (someFunc)

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
  sendAll s "Hello, world!"
  msg <- recv s 1024
  putStr "Received: "
  C.putStrLn msg

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock
