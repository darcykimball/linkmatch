{-# LANGUAGE OverloadedStrings #-}
module Subscriber where


import Control.Monad
import Data.Aeson

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE

import Network.Simple.TCP

import Event
import EqMatchTree
import Message


-- Subscribe to some content, and wait for events forever
runSubscriber ::
  [Predicate] ->
  HostName -> -- Subscriber IP
  ServiceName -> -- Subscriber portt
  HostName -> -- Broker IP
  ServiceName -> -- Broker port
  IO ()
runSubscriber preds myIP myPort brokerIP brokerPort = do
  -- Send out subscription request
  connect brokerIP brokerPort $ \(sock, _) ->
    send sock $ LB.toStrict $ encode (SubMsg preds)

  -- Wait for events...
  forever $ do
    listen (Host myIP) myPort $ \(sock, _) -> do
      accept sock $ \(recvSock, _) -> do
        mBytes <- recv recvSock 4096

        maybe
          (T.putStrLn "Received a message that was too big! Ignoring...")
          handleReceived
          mBytes 


handleReceived :: B.ByteString -> IO ()
handleReceived bytes = 
  either
    print
    (\msg -> do
      T.putStrLn "Got a message:"
      T.putStrLn msg
    )
    (validateReceived bytes)


validateReceived :: B.ByteString -> Either PubSubError T.Text
validateReceived bytes =
  maybe
    (Left MalformedMsg)
    (Right . fwdMsg)
    m

  where
    mMsg = decode $ LB.fromStrict bytes :: Maybe Msg
    m = mMsg >>= isFwd

    isFwd f@(FwdMsg _) = Just f
    isFwd _ = Nothing
