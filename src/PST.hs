module PST where


import Data.List.NonEmpty

import Data.Text as T

import qualified Data.Set as S


data PST sub =
    Empty
  | Branch (NonEmpty (Result, PST sub))
  | Leaf (S.Set sub)
  deriving (Eq, Ord, Show)


data Result =
    Value Text
  | DontCare
  deriving (Eq, Ord, Show)
