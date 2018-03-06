module Set (
    Set,

    empty,
    isEmpty,

    singleton,

    size,

    toList,
    fromList,

    split,

    member,
    insert,
    delete,
    union,
    intersection,
    difference,

    isSubsetOf,

    mapSet,
    filterSet
  ) where

import Data.List (sort)

-- | We represent sets as *ordered* lists without duplicates
data Set a = Set [a]
  deriving (Eq, Ord, Show, Read)

-- | Return the empty set.
empty :: Set a
empty = Set []

-- | Return @True@ if the set is empty, @False@ otherwise.
isEmpty :: Set a -> Bool
isEmpty (Set xs) = null xs

-- | Create a set containing only the given item.
singleton :: a -> Set a
singleton x = Set [x]

-- | Return the number of items in the set.
size :: Set a -> Int
size (Set xs) = length xs

-- | Return a list containing only the items in a set. Order is unspecified.
toList :: Set a -> [a]
toList (Set xs) = xs

-- | Create a set containing only the items in the given list.
fromList :: Ord a => [a] -> Set a
fromList = Set . nub . sort
  where
    nub :: Eq a => [a] -> [a]
    nub []                   = []
    nub [x]                  = [x]
    nub (x:y:ys) | x == y    = nub (y:ys)
                 | otherwise = x : nub (y:ys)

-- | Split a non-empty set into a pair, the first element of which is an item in
-- the set, and the second element of which is the rest of the set.
split :: Set a -> (a, Set a)
split (Set [])     = error "Cannot split empty set"
split (Set (x:xs)) = (x, Set xs)

-- | Return @True@ if the given item is a member of the set, @False@ otherwise.
member :: Ord a => a -> Set a -> Bool
member xs0 (Set ys0) = mem xs0 ys0
  where
    mem :: Ord a => a -> [a] -> Bool
    mem _ []                 = False
    mem x (y:ys) | x < y     = False
                 | x == y    = True
                 | otherwise = mem x ys

-- | Insert an item into the set.
insert :: Ord a => a -> Set a -> Set a
insert x0 (Set ys0) = Set (ins x0 ys0)
  where
    ins :: Ord a => a -> [a] -> [a]
    ins x []                 = [x]
    ins x (y:ys) | x < y     = x : (y : ys)
                 | x == y    = y : ys
                 | otherwise = y : ins x ys

-- | Delete an item from the set.
delete :: Ord a => a -> Set a -> Set a
delete x0 (Set ys0) = Set (del x0 ys0)
  where
    del :: Ord a => a -> [a] -> [a]
    del _ []                 = []
    del x (y:ys) | x < y     = y : ys
                 | x == y    = ys
                 | otherwise = y : del x ys

-- | Return the union of two sets, i.e., the set whose elements are members
-- of either one set or the other.
union :: Ord a => Set a -> Set a -> Set a
union (Set xs0) (Set ys0) = Set (uni xs0 ys0)
  where
    uni :: Ord a => [a] -> [a] -> [a]
    uni []     ys                 = ys
    uni xs     []                 = xs
    uni (x:xs) (y:ys) | x < y     = x : uni xs (y:ys)
                      | x == y    = x : uni xs ys
                      | otherwise = y : uni (x:xs) ys

-- | Return the intersection of two sets, i.e., the set whose elements are members
-- of both sets.
intersection :: Ord a => Set a -> Set a -> Set a
intersection (Set xs0) (Set ys0) = Set (int xs0 ys0)
  where
    int :: Ord a => [a] -> [a] -> [a]
    int []     _                  = []
    int _      []                 = []
    int (x:xs) (y:ys) | x < y     = int xs (y:ys)
                      | x == y    = x : int xs ys
                      | otherwise = int (x:xs) ys

difference ::  Ord a => Set a -> Set a -> Set a
difference (Set xs0) (Set ys0) = Set (diff xs0 ys0)
  where
    diff :: Ord a => [a] -> [a] -> [a]
    diff []     _                  = []
    diff xs     []                 = xs
    diff (x:xs) (y:ys) | x < y     = x : diff xs (y:ys)
                       | x == y    = diff xs ys
                       | otherwise = diff (x:xs) ys

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf (Set xs0) (Set ys0) = subs xs0 ys0
  where
    subs :: Ord a => [a] -> [a] -> Bool
    subs []     _                  = True
    subs _      []                 = False
    subs (x:xs) (y:ys) | x < y     = False
                       | x == y    = subs xs ys
                       | otherwise = subs (x:xs) ys

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = fromList (map f xs)

filterSet :: Ord a => (a -> Bool) -> Set a -> Set a
filterSet f (Set xs) = Set (filter f xs)
