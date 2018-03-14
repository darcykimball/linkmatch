module Publisher where


import Data.Aeson
import Network.Simple.TCP

import qualified Data.ByteString.Lazy as LB

import Message
import Event


type EventSource = IO (Maybe PubMsg)


-- Run a publisher, calling upon a provided event source to get messages to
-- publish.
runPublisher ::
  EventSchema ->
  HostName ->
  ServiceName ->
  EventSource ->
  IO ()
runPublisher schema brokerIP brokerPort eventSrc =
  eventSrc >>= 
    maybe 
      (return ())
      (\p@(PubMsg msg attrs) -> do
        let attrsOk = and $ map (attrIsConsistent schema) attrs
        if not attrsOk
          then putStrLn "inconsistent attributes in event, ignoring."
          else publish p brokerIP brokerPort
      )
      

publish :: PubMsg -> HostName -> ServiceName -> IO ()
publish event brokerIP brokerPort =
  connect brokerIP brokerPort $
    \(sock, _) -> send sock $ LB.toStrict $ encode event
