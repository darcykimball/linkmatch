{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Topo where


import Data.Aeson
import GHC.Generics
import Network.Simple.TCP

import Event


-- Broker  rose tre 
data BrokerTopo sub = BrokerTopo {
    rootIP :: HostName
  , children :: [BrokerTopo sub]
  , subIPs  :: [(HostName, sub)]
  }
  deriving (Show, Eq, Ord, Generic)


instance FromJSON sub => FromJSON (BrokerTopo sub)
instance ToJSON sub => ToJSON (BrokerTopo sub)
