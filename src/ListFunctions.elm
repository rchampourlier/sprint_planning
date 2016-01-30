module ListFunctions where

import List exposing (filter, member)

-- Removes item present in the second list from the first
-- one.
substract : List a -> List a -> List a
substract list1 list2 =
  filter (\i -> not (member i list2)) list1

-- Returns an indexed list, where each item of the original
-- list is mapped to a 2-uple with an integer index.
indexList : List a -> Int -> List (Int, a)
indexList list startIndex =
  case list of
    head :: tail -> (startIndex, head) :: indexList tail (startIndex + 1)
    _ -> []
