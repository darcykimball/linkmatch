{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
-- Pub/sub message types 
module Message where


import Data.Aeson
import GHC.Generics

import qualified Data.Text as T

import EqMatchTree
import Event

--
-- Raw message data types
--

-- TODO

--
-- Message payload types
--


-- A publisher message
data PubMsg = PubMsg {
    pubMsg   :: T.Text
  , pubAttrs :: [Attr]  
  } deriving (Generic, Show, Eq, Ord)


-- A subscriber message
data SubMsg = SubMsg {
    subPreds :: [Predicate]
  } deriving (Generic, Show, Eq, Ord)


-- A broker forwarding msg
data FwdMsg = FwdMsg {
    fwdMsg :: T.Text
  } deriving (Generic, Show, Eq, Ord)


-- Instances/Derivations
deriving instance Generic Attr
deriving instance Generic AttrValue
instance ToJSON Attr
instance ToJSON AttrValue
instance ToJSON PubMsg
instance ToJSON SubMsg
instance ToJSON FwdMsg
instance FromJSON Attr
instance FromJSON AttrValue
instance FromJSON PubMsg
instance FromJSON SubMsg
instance FromJSON FwdMsg


-- Exception type
data PubSubError =
    MalformedMsg
  | UnreachableSub
  | UnreachablePub
  | UnreachableBroker
  deriving (Show, Eq, Ord)
