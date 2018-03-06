module RegExpMatch (regExpMatch) where

import RegExp

-- regExpMatch :: ?
regExpMatch _ _ = error "regExpMatch unimplemented"

-- Returns all the ways of splitting a list into two halves.
splits :: [a] -> [([a], [a])]
splits st = [splitAt n st | n <- [0 .. length st]]

-- Returns all the ways of splitting a list into two halves where the first
-- split is non-trivial
frontSplits :: [a] -> [([a], [a])]
frontSplits st = [splitAt n st | n <- [1.. length st]]
