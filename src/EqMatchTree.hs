{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module EqMatchTree where


import Control.Arrow (second)
import Control.Monad
import Data.List
import Data.Semigroup ((<>))

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

import Event


data EqMatchTree subid =
    Empty
  -- The set of subidscribers for this pred. path
  | Leaf (NE.NonEmpty subid) 
  -- Outgoing edges are either an attribute value to test equality with, or
  -- Nothing, representing (*).
  | Branch Attr (NE.NonEmpty (Maybe AttrValue, EqMatchTree subid))
  deriving (Eq, Ord, Show, Functor)



-- A predicate is just a conjunction of equality tests on message attributes,
-- so here it's represented as just attribute values. The absence of an
-- attribute when processing implies that its test value is (*), i.e. don't
-- care. 
type Predicate = [Attr]


empty :: EqMatchTree subid
empty =  Empty


-- Build a matching tree. If supplied predicates are inconsistent with the
-- supplied schema, returns Nothing.
buildFromPredicates ::
  EventSchema ->
  [(subid, [Predicate])] ->
  Maybe (EqMatchTree subid)
buildFromPredicates schema subidPreds =
  foldM
    (\tree (subid, preds) -> foldM (\t p -> addSub p subid t) tree preds)
    Empty
    subidPreds 
  where
    addSub = addSubscriberPred schema


-- Add a subscriber to the matching tree. If supplied predicates are
-- inconsistent with the supplied schema, returns Nothing
addSubscriberPred :: forall subid.
  EventSchema ->
  Predicate ->
  subid ->
  EqMatchTree subid ->
  Maybe (EqMatchTree subid)
addSubscriberPred schema pred subid tree = do
  guard $ predIsConsistent schema pred

  maybe
    Nothing
    (\c -> insert schemaAttrs c tree)
    canonicalized 

  where
    schemaAttrs = M.assocs schema -- stack of sorted schema attrs
    canonicalized = canonPred schema pred -- assoc. list of tests to results
    
    insert ::
      [(AttrName, AttrValue)] ->
      [(AttrName, Maybe AttrValue)] ->
      EqMatchTree subid ->
      Maybe (EqMatchTree subid)
    insert (a:as) (t:ts) curr =
      case curr of
        Empty -> do
          child <- insert as ts Empty

          let val = maybe (Str "") defaultVal mVal
              attr = Attr name val
          return $ Branch attr ((mVal, child) NE.:| [])
        
        Leaf subs -> return $ Leaf $ NE.cons subid subs
        
        Branch nm children ->
          if null matchedEdges

            then do 
              child <- insert as ts Empty
              return $ Branch nm ((Nothing, child)  NE.<| children)

            else do
              matchedChildren <- forM matchedEdges $ \(val, child) -> do
                                    newChild <- insert as ts child
                                    return (val, child)

              return $
                Branch nm (NE.fromList matchedChildren <> NE.fromList restEdges)

          where
            (matchedEdges, restEdges) = NE.partition ((== mVal) . fst) children

      where 
        (name, mVal) = t
      
    insert [] [] t = return t
    insert _  _  _ = error "Canonicalized predicate doesn't match schema!"


lookupSubscribers :: [Attr] -> EqMatchTree subid -> [subid]
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
canonPred schema pred = do
  guard $ predIsConsistent schema pred
  return canonicalized
  where
    assocPred = map toP pred
    canonicalized = sortOn fst $ map makeTest $ M.keys schema
    makeTest name =
      maybe
        (name, Nothing)
        (\val -> (name, Just val))
        (lookup name assocPred)


-- Utility fn...
toP (Attr name val) = (name, val)
