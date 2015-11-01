module ListFunctions where

import List exposing (filter, member)
import Set

-- Removes item present in the second list from the first
-- one.
substract : List a -> List a -> List a
substract list1 list2 =
  filter (\i -> not (member i list2)) list1
