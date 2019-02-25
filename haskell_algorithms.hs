import Data.Char (ord, chr, isLower, isUpper, isLetter)

-- A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
perfects' :: Int -> [Int]
factors' n = [x | x <- [1..n-1], n `mod` x == 0]
perfects' n = [x | x <- [1..n-1], x == sum(factors' x)]

-- Caesar Cipher Program
let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
let2int c | isUpper c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

int2let' :: Int -> Char
int2let' n = chr(ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
shift n c | isUpper c = int2let' ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Caesar Cipher Program 2nd implementation
int_letter :: Char -> [(Int, Char)]
int_letter c | isLower c = zip [0..25] ['a'..'z']
int_letter c | isUpper c = zip [0..25] ['A'..'Z']

findIntValue :: Char -> Int
findIntValue c = head [a | (a, b) <- int_letter c, b == c]

getNewValue :: Int -> Char -> Int
getNewValue n c = (findIntValue c + n) `mod` 26

getNewChar :: Int -> Char -> Char
getNewChar n c | isLetter c = head [b | (a, b) <- int_letter c, a == getNewValue n c]
               | otherwise = c

cipher :: Int -> String -> String
cipher n cs = [getNewChar n x | x <- cs]

-- Safetail function that behaves as the library function tail, except that safetail
-- maps to empty list to itself.
safetail' :: Eq a => [a] -> [a]
safetail' a | a == [] = a
            | otherwise = tail a

-- Scalar Product, scalarProduct [1, 2, 3] [4, 5, 6] = 1*4 + 2*5 + 3*6 = 32
scalarProduct :: [Int] -> [Int] -> Int
scalarProductList a b = if length a > length b
  then [a!!n * b!!n | n <- [0..length b - 1]]
  else [a!!n * b!!n | n <- [0..length a - 1]]
scalarProduct a b = sum (scalarProductList a b)

-- Scalar Prodcut 2nd implementation
scalarProduct' :: [Int] -> [Int] -> Int
scalarProduct' xs ys = sum [k * v | (k, v) <- zip xs ys]

-- You are given a function plusOne x = x + 1. Without using any other (+)s,
-- define a recursive function addition such that addition x y adds x and y together.
plusOne :: Int -> Int
plusOne x = x + 1

add' :: Int -> Int -> Int
add' x 0  = x
add' 0 y = y
add' x y = add' (plusOne x) (y - 1)

-- Implement the function log2, which computes the integer log (base 2) of its argument.
-- That is, log2 computes the exponent of the largest power of 2 which is less than or equal to its
-- argument.
log2 :: Int -> Int
log2 a = last[n | n <- [1..30], a >= 2^n ]

-- Second implementation of log2
log2' :: Int -> Int
log2' 0 = 0
log2' 1 = 0
log2' a = 1 + log2' (div a 2)
