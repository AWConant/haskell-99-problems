import Data.List hiding (insert)
import Data.Map.Strict (Map, elems, member, toList, adjust, insert, empty)

-- Problem 1
-- Find the last element of list.
last' :: [a] -> a
last' [] = error "tried to call last' with empty list"
last' [x] = x
last' (_:xs) = last' xs

-- Problem 2
-- Find the last but one element of a list.
butLast' :: [a] -> a
butLast' [] = error "tried to call butLast' with empty list"
butLast' [_] = error "tried to call butLast' with 1-length list"
butLast' (x:_:[]) = x 
butLast' (_:x:xs) = butLast' (x:xs)
  
-- Problem 3 
-- Find the K'th element of a list. The first element in the list is number 1.
kthElem' :: [a] -> Int -> a
kthElem' [] _ = error "tried to call kthElem' with empty list" 
kthElem' list i
  | i > length list || i < 1 = error "index out of bounds in kthElem'"
kthElem' list@(x:xs) i = kthHelper' list i 1 

kthHelper' :: [a] -> Int -> Int -> a
kthHelper' (x:xs) i j
  | i == j = x
  | otherwise = kthHelper' xs i (j + 1)

-- Problem 4 
-- Find the number of elements in a list.
listLen' :: [a] -> Int
listLen' [] = 0
listLen' (_:xs) = 1 + listLen' xs

-- Problem 5 
-- Reverse a list.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

-- Problem 6 
-- Find out whether a list is a palindrome. A palindrome can be read forward or
-- backward; e.g. [1, 2, 3, 4, 5]
palindrome' :: (Eq a) => [a] -> Bool
palindrome' [] = True
palindrome' [_] = True
palindrome' xs
  | head xs == last xs = palindrome' $ init $ tail xs
  | otherwise = False

-- Problem 7 
-- Flatten a nested list stucture; e.g.
-- *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1, 2, 3, 4, 5]
data NestedList a = Elem a | List [NestedList a]
flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List []) = []
flatten' (List (x:xs)) = (flatten' x) ++ (flatten' (List xs))

-- Problem 8 
-- Eliminate consecute duplicates of list elements.
compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' [x] = [x]
compress' (x:ys@(y:_)) 
  | x == y = compress' ys 
  | otherwise = x:(compress' ys)

-- Problem 9 
-- Pack consecutive duplicate elements in sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) =
  let packed = pack xs in
  if x `elem` (head packed)
  then (x:(head packed)):(tail packed)
  else [x]:packed

-- Problem 10
-- Run-length encoding of a list. Use the result of Problem 9 to implement this.
-- Consecutive duplicates of elements are encoded as tuples of the form (N, E)
-- where N denotes the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map encode_one $ pack xs
  where
    encode_one = (\x -> (listLen' x, head x))

-- Problem 11
-- Modified run-length encoding. Modify the result of Problem 10 such that if
-- an element has no duplicates it is simply copied into the result list. Only
-- elements with duplicates are transferred as (N, E) tuples.
data RunLength a = Multiple Int a | Single a deriving Show
encodeMod :: (Eq a) => [a] -> [RunLength a]
encodeMod = map encode_one . encode 
  where
    encode_one (1, v) = Single v
    encode_one (n, v) = Multiple n v 

-- Problem 12
-- Decode a run-length encoded (as described in Problem 11) list.
decodeMod :: (Eq a) => [RunLength a] -> [a]
decodeMod xs = concatMap decode_one $ xs
  where
    decode_one (Single v) = [v]
    decode_one (Multiple n v) = replicate n v 
        

-- Problem 13
-- Run-length encoding of a list (direct solution). Implement the run-length
-- encoding data compression method directly, meaning do not create sublists
-- like you did in Problem 9, merely count duplicates. As in Problem 11, 
-- simplify the result list by replacing the singleton lists with Singles.
encodeDirect :: (Eq a) => [a] -> [RunLength a]
encodeDirect xs = map encode_one $ pack xs
  where
    encode_one x
      | n == 1 = Single v
      | otherwise = Multiple n v
        where
          n = listLen' x
          v = head x
          

-- Problem 14
-- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- Problem 15
-- Replicate the elements of a list a given number of times.
repli :: Int -> [a] -> [a]
repli n = concatMap (replicate n)

-- Problem 16
-- Drop every N'th element from a list.
dropEvery :: Int -> [a] -> [a]
dropEvery n xs = map snd . filter dropNth $ zip [1,2..] xs
  where
    dropNth = (\t -> if fst t `mod` n == 0 then False else True)

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split [] _  = ([], [])
split l@(x:xs) n
  | n == 0 = ([], l)
  | otherwise = (x:ys, zs)
    where
      (ys, zs) = split xs (n - 1)

-- Problem 18
-- Extract a slice from a list given two indices i and k, where the slice is
-- the list of elements between elements i and k (inclusive).
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) i k
  | k == 1 = [x]
  | i == 1 = x : slice xs i (k - 1)
  | otherwise = slice xs (i - 1) (k - 1)

-- Problem 19
-- Rotate a list N places to the left. Accomodate positive and negative inputs.
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate l@(x:xs) n
  | n == 0 = l
  | n > 0 = rotate (xs ++ [x]) (n - 1)
  | n < 0 = rotate ((last l):(init l)) (n + 1)
  

-- Problem 20
-- Remove the K'th element from a list. Return a tuple (R, L) such that R is
-- the removed element and L is the new list.
removeAt :: [a] -> Int -> (a, [a])
removeAt [] _ = error "can't removeAt on an empty list"
removeAt (x:xs) i
  | i == 1 = (x, xs)
  | otherwise = (v, x:ys)
    where 
      (v, ys) = removeAt xs (i - 1)

-- Problem 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt v (x:xs) i 
  | i == 1 = v:x:xs 
  | otherwise = x:insertAt v xs (i - 1)

-- Problem 22
-- Create a list containing all integers within a given range (inclusive).
range :: Integral a => a -> a -> [a]
range i k 
  | i == k = [k]
  | otherwise = i : range (i + 1) k

-- Problem 23
-- Extract a given number of randomly selected elements from a list.

-- Problem 24
-- Draw N different random numbers from the set 1..M

-- Problem 25
-- Generate a random permutation of the elements of a list.

-- Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements
-- of a list.

-- Problem 27
-- Group the elements of a set into disjoint subsets.

-- Problem 28
-- Group the elements of a set into disjoint subsets.

-- Problem 29
-- Sorting a list of lists according to length of sublists:
-- a) Short lists first, longer lists later.
lsort :: [[a]] -> [[a]]
lsort xs = sortBy (\xs ys -> compare (length xs) (length ys)) xs

-- Problem 30
-- b) Rare list lengths first, longer list lengths later.
type Index = Int
type Length = Int
lfsort :: [[a]] -> [[a]]
lfsort xs = map (xs !!) (foldl' (++) [] withoutIndices)
  where
    sublistLength (_, xs) (_, ys) = compare (length xs) (length ys)
    lenMap = enumerateLengths (zip [0,1..] xs) empty
    sortedLens = sortBy sublistLength (toList lenMap)
    withoutIndices = map snd sortedLens

enumerateLengths :: [(Index, [a])] -> Map Length [Index] -> Map Length [Index]
enumerateLengths [] m = m
enumerateLengths ((i, x):xs) m =
  if xlen `member` m
  then enumerateLengths xs (adjust (i : ) xlen m)
  else enumerateLengths xs (insert xlen [i] m)
    where
      xlen = length x

-- Problem 31
-- Determine whether a given number is prime.
isPrime :: Integral a => a -> Bool
isPrime n = all (/= 0) $ map (mod n) [2..truncate $ sqrt $ fromIntegral n]

-- Problem 32
-- Determine the greatest common divisor of two positive integers
myGCD :: Integral a => a -> a -> a
myGCD a b 
  | b == 0 = a
  | otherwise = myGCD b (a `mod` b)

-- Problem 33
-- Determine whether two positive integers are coprime. Two numbers are coprime
-- if their greatest common divisor equals 1.
coprime :: Integral a => a -> a -> Bool
coprime a b 
  | myGCD a b == 1 = True
  | otherwise = False

-- Problem 34
-- Calculate Euler's totient function phi(m)
-- Euler's so-called totient function phi(m) is defined as the number of
-- positive intgers r (1 <= r < m) that are coprime to m. 
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4.
-- phi(1) = 1
totient :: Integral a => a -> Int
totient m = length $ filter (coprime m) [1..m]

-- Problem 35
-- Determine the prime factors of a given positive integer. Construct a flat
-- list containing the prime factors in ascending order.
primeFactors :: Integral a => a -> [a]
primeFactors n =
  let primes = filter isPrime [2..n] in
  primeIter n primes [] 

primeIter :: Integral a => a -> [a] -> [a] -> [a]
primeIter n _ factors
  | n == 1 = reverse factors
primeIter n primes@(p:ps) factors =
  if n `mod` p == 0
  then primeIter (n `quot` p) primes (p:factors)
  else primeIter n ps factors

-- Problem 36

-- Problem 37

-- Problem 38

-- Problem 39

-- Problem 40

-- Problem 41

-- Problem 42

-- Problem 43

-- Problem 44

-- Problem 45

-- Problem 46

-- Problem 47

-- Problem 48

-- Problem 49

-- Problem 50

-- Problem 51

-- Problem 52

-- Problem 53

-- Problem 54

-- Problem 55

-- Problem 56

-- Problem 57

-- Problem 58

-- Problem 59

-- Problem 60

-- Problem 61

-- Problem 62

-- Problem 63

-- Problem 64

-- Problem 65

-- Problem 66

-- Problem 67

-- Problem 68

-- Problem 69

-- Problem 70

-- Problem 71

-- Problem 72

-- Problem 73

-- Problem 74

-- Problem 75

-- Problem 76

-- Problem 77

-- Problem 78

-- Problem 79

-- Problem 80

-- Problem 81

-- Problem 82

-- Problem 83

-- Problem 84

-- Problem 85

-- Problem 86

-- Problem 87

-- Problem 88

-- Problem 89

-- Problem 90

-- Problem 91

-- Problem 92

-- Problem 93

-- Problem 94

-- Problem 95

-- Problem 96

-- Problem 97

-- Problem 98

-- Problem 99
