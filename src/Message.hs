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


data Msg =
    -- A publisher message
    PubMsg {
      pubMsg   :: T.Text
    , pubAttrs :: [Attr]  
    }
    -- A subscriber message
  | SubMsg {
      subPreds :: [Predicate]
    } 
  -- A broker forwarding msg
  | FwdMsg {
      fwdMsg :: T.Text
    }
  deriving (Generic, Show, Eq, Ord)


-- Instances/Derivations
deriving instance Generic Attr
deriving instance Generic AttrValue
instance ToJSON Attr
instance ToJSON AttrValue
instance ToJSON Msg
instance FromJSON Attr
instance FromJSON AttrValue
instance FromJSON Msg


-- Exception type
data PubSubError =
    MalformedMsg
  | UnreachableSub
  | UnreachablePub
  | UnreachableBroker
  deriving (Show, Eq, Ord)
