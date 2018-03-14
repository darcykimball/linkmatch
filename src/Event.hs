{-# LANGUAGE OverloadedStrings #-}
module Event where


import Network.Simple.TCP

import qualified Data.Map as M
import qualified Data.Text as T


sUB_PORT :: ServiceName
sUB_PORT = "9999"


bROKER_PORT :: ServiceName
bROKER_PORT = "8888"

-- An event attribute
data Attr = Attr {
    _attrName :: AttrName
  , _attrVal  :: AttrValue
  }
  deriving (Show, Eq, Ord)


-- Attribute names are strings
type AttrName = T.Text


-- Possible attribute value types
-- XXX: For purposes of defining schema, AttrValue's are also used, but with
-- dummy values, i.e. the empty string or 0. They're ignored in places where
-- schemas are used.
data AttrValue =
    Str T.Text
  | Number Int
  deriving (Show, Eq, Ord)


-- Event schema
type EventSchema = M.Map AttrName AttrValue


makeSchemaAttr :: Attr -> Attr
makeSchemaAttr attr@(Attr _ val) = attr { _attrVal = defaultVal val }


defaultVal :: AttrValue -> AttrValue
defaultVal (Str _) = Str ""
defaultVal (Number _) = Number 0


attrIsConsistent :: EventSchema -> Attr -> Bool
attrIsConsistent schema (Attr name val) =
  M.lookup name schema == Just (defaultVal val)


-- A published event, ready for processing
data Event = Event {
    _msg   :: T.Text
  , _attrs :: [Attr]
  }
  deriving (Show, Eq, Ord)
