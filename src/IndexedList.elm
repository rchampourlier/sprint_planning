module IndexedList where

-- Removes item present in the second list from the first
-- one.
append : List (Int, a) -> a -> List (Int, a)
append list newValue =
  case list of
    [] -> [(0, newValue)]
    head :: [] ->
      case head of
        (index, value) -> [(index, value)] ++ [(index + 1, newValue)]
    head :: tail -> [head] ++ (append tail newValue)
