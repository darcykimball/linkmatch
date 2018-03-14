{-# LANGUAGE OverloadedStrings #-}
module Event where


import qualified Data.Map as M
import qualified Data.Text as T


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
  where
    defaultVal (Str _) = Str ""
    defaultVal (Number _) = Number 0


-- A published event, ready for processing
data Event = Event {
    _msg   :: T.Text
  , _attrs :: [Attr]
  }
  deriving (Show, Eq, Ord)
