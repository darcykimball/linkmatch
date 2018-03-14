module Broker where


import Network.Simple.TCP

import EqMatchTree
import Event
import Message
import Topo


runBroker ::
  HostName -> -- My IP
  ServiceName -> -- My port (pub/sub service)
  BrokerTopo Predicate -> -- Broker topology
  IO ()
runBroker myIP myPort topo = do
  undefined
