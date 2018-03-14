{-# LANGUAGE OverloadedStrings #-}
module Publisher where


import Control.Concurrent
import Control.Monad
import Data.Aeson
import Network.Simple.TCP

import qualified Data.ByteString.Lazy as LB

import Message
import Event


-- A stream of events is simulated by list of msgs, waiting a bit between
-- each publish
type EventSource = [Msg]


-- Run a publisher, calling upon a provided event source to get messages to
-- publish.
runPublisher ::
  EventSchema ->
  HostName ->
  ServiceName ->
  EventSource ->
  IO ()
runPublisher schema brokerIP brokerPort eventSrc =
  forM_ eventSrc $ \event -> do
    threadDelay 500    

    let p@(PubMsg msg attrs) = event
        attrsOk = and $ map (attrIsConsistent schema) attrs

    if not attrsOk
      then putStrLn "inconsistent attributes in event, ignoring."
      else publish p brokerIP brokerPort
      

publish :: Msg -> HostName -> ServiceName -> IO ()
publish event brokerIP brokerPort =
  connect brokerIP brokerPort $
    \(sock, _) -> send sock $ LB.toStrict $ encode event


ringForever :: EventSource
ringForever = repeat ring


ringN :: Int -> EventSource
ringN n = replicate n ring


ring :: Msg
ring = PubMsg ("ring ring ring") [Attr "bell" (Str "loud")] 
