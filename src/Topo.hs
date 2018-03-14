{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Topo where


import Data.Aeson
import GHC.Generics
import Network.Simple.TCP

import Event
import EqMatchTree



-- Broker  rose tre 
data BrokerTopo sub = BrokerTopo {
    rootIP :: HostName
  , children :: [BrokerTopo sub]
  , subIPs  :: [(HostName, sub)]
  }
  deriving (Show, Eq, Ord, Generic)


instance FromJSON sub => FromJSON (BrokerTopo sub)
instance ToJSON sub => ToJSON (BrokerTopo sub)


getSubs :: BrokerTopo sub -> [(HostName, sub)]
getSubs BrokerTopo{..} =
  subIPs ++ concatMap getSubs children


buildMatchTreeFromTopo ::
  EventSchema -> BrokerTopo Predicate -> Maybe (EqMatchTree HostName)
buildMatchTreeFromTopo schema topo =
  buildFromPredicates schema $ getSubs topo
     
