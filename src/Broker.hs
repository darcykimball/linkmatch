{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Broker where


import Control.Monad
import Network.Simple.TCP

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

import EqMatchTree
import Event
import Message
import Topo


-- Run with only matching tree: no matching tree annotations
runBrokerSimple ::
  HostName -> -- My IP
  ServiceName -> -- My port (pub/sub service)
  BrokerTopo Predicate -> -- Broker topology
  IO ()
runBrokerSimple myIP myPort BrokerTopo{..} = forever $ do
  listen (Host myIP) myPort $ \(sock, _) -> do
    accept sock $ \(recvSock, _) -> do
      mBytes <- recv recvSock 4096 

      maybe
        (T.putStrLn "Received a message that was too big! Ignoring...")
        handleReceived
        mBytes


handleReceived :: B.ByteString -> IO ()
handleReceived = undefined


handleMsg :: Msg -> IO ()
handleMsg = undefined


parseReceived :: B.ByteString -> Msg 
parseReceived bytes = undefined
