{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Broker where


import Control.Monad
import Data.Aeson
import Data.Maybe
import Network.Simple.TCP

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.IO as T

import EqMatchTree
import Event
import Message
import Topo


-- Run with only matching tree: no matching tree annotations
runBrokerSimple ::
  EventSchema ->
  HostName -> -- My IP
  ServiceName -> -- My port (pub/sub service)
  BrokerTopo Predicate -> -- Broker topology
  IO ()
runBrokerSimple schema myIP myPort topo = forever $ do
  if isNothing mTree then error "Bad topo/tree!" else return ()

  let Just tree = mTree
  listen (Host myIP) myPort $ \(sock, _) -> do
    accept sock $ \(recvSock, _) -> do
      mBytes <- recv recvSock 4096 

      maybe
        (T.putStrLn "Received a message that was too big! Ignoring...")
        (\bytes -> handleReceived bytes tree)
        mBytes
   where
      mTree = buildMatchTreeFromTopo schema topo


handleReceived :: B.ByteString -> EqMatchTree HostName -> IO ()
handleReceived bytes tree = 
  maybe
    (T.putStrLn "Bad JSON in msg, ignoring...")
    handleMsg
    mMsg
  where
    mMsg :: Maybe Msg
    mMsg = decode $ LB.fromStrict bytes


    handleMsg :: Msg -> IO ()
    handleMsg PubMsg{..} = do
        forM_ subs $ \s -> do
          let bytes = LB.toStrict $ encode $ FwdMsg pubMsg
          connect s sUB_PORT $ \(sock, _) -> send sock bytes
      where
        subs = lookupSubscribers pubAttrs tree
      
    
    handleMsg SubMsg{..} = do
      -- Since w're assuming the topology is already set (for our purposes),
      -- this is a no-op
      T.putStrLn "Got sub msg!"
    handleMsg _           = T.putStrLn "Bad msg! Ignoring..."
