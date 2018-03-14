{-# LANGUAGE DeriveFunctor #-}
module EqMatchTree where


import Data.List

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

import Event


data EqMatchTree sub =
    Empty
  -- The set of subscribers for this pred. path
  | Leaf (NE.NonEmpty sub) 
  -- Outgoing edges are either an attribute value to test equality with, or
  -- Nothing, representing (*).
  | Branch Attr (NE.NonEmpty (Maybe AttrValue, EqMatchTree sub))
  deriving (Eq, Ord, Show, Functor)



-- A predicate is just a conjunction of equality tests on message attributes,
-- so here it's represented as just attribute values. The absence of an
-- attribute when processing implies that its test value is (*), i.e. don't
-- care. 
type Predicate = [Attr]


empty :: EqMatchTree sub
empty =  Empty


-- Build a matching tree. If supplied predicates are inconsistent with the
-- supplied schema, returns Nothing.
buildFromPredicates ::
  EventSchema ->
  [(sub, Predicate)] ->
  Maybe (EqMatchTree sub)
buildFromPredicates schema subPreds = foldl' addSubPred (Just Empty) subPreds
  where
    addSubPred mTree (sub, pred) = mTree >>= addSubscriberPred schema pred sub


-- Add a subscriber to the matching tree. If supplied predicates are
-- inconsistent with the supplied schema, returns Nothing
addSubscriberPred ::
  EventSchema ->
  Predicate ->
  sub ->
  EqMatchTree sub ->
  Maybe (EqMatchTree sub)
addSubscriberPred schema pred sub tree
  | predIsConsistent schema pred = Nothing
  | otherwise = undefined
      where
        schemaAttrs = M.assocs schema -- stack of sorted schema attrs
        tests = sort $ map toP pred -- assoc. list of tests to results
        
        -- Easier to just canonicalize: TODO


lookupSubscribers :: [Attr] -> EqMatchTree sub -> [sub]
lookupSubscribers attrs tree =
  case tree of
    _ -> undefined 
  where
    sorted = sortOn _attrName attrs
      

-- Check if a predicate is consistent with a schema
predIsConsistent :: EventSchema -> Predicate -> Bool
predIsConsistent schema attrs =
  M.intersection schema schemaSubset == schemaSubset
  where
    schemaSubset = M.fromList $ map (toP . makeSchemaAttr) attrs


-- Check if a predicate is ok with a schema, and add wildcards if necessary
canonPred :: EventSchema -> Predicate -> Maybe [(AttrName, Maybe AttrValue)]
canonPred schema pred =
  if predIsConsistent schema pred
    then Nothing
    else Just canonicalized
  where
    assocPred = map toP pred
    canonicalized = map makeTest $ M.keys schema
    makeTest name =
      maybe
        (name, Nothing)
        (\val -> (name, Just val))
        (lookup name assocPred)


-- Utility fn...
toP (Attr name val) = (name, val)
