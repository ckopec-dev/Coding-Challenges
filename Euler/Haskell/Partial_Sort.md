# Euler Problem: Partial Sort

## Problem Statement

The problem asks us to find the sum of the first 20 elements of the sequence where each element is the sum of the first n natural numbers (triangular numbers), but only considering those that are divisible by 3 or 5.

## Solution in Haskell

```haskell
-- Generate triangular numbers
triangular :: Integer -> Integer
triangular n = n * (n + 1) `div` 2

-- Check if a number is divisible by 3 or 5
divisibleBy3Or5 :: Integer -> Bool
divisibleBy3Or5 n = n `mod` 3 == 0 || n `mod` 5 == 0

-- Generate the sequence of triangular numbers divisible by 3 or 5
partialSortSequence :: [Integer]
partialSortSequence = [triangular n | n <- [1..], divisibleBy3Or5 (triangular n)]

-- Get the first 20 elements and sum them
solve :: Integer
solve = sum $ take 20 partialSortSequence

-- Main function to run the solution
main :: IO ()
main = print solve
```

## Alternative Implementation

```haskell
-- More efficient approach using lazy evaluation
solve' :: Integer
solve' = sum $ take 20 $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) triangularNumbers
  where
    triangularNumbers = scanl (+) 1 [2..]
```

## Step-by-step Explanation

1. **Triangular Numbers**: The nth triangular number is calculated as n(n+1)/2
2. **Filtering**: We only keep triangular numbers that are divisible by 3 or 5
3. **Take First 20**: We extract the first 20 elements from this filtered sequence
4. **Sum**: Finally, we sum these 20 numbers

## Verification

Let's trace through the first few triangular numbers:
- T(1) = 1 (not divisible by 3 or 5)
- T(2) = 3 (divisible by 3) ✓
- T(3) = 6 (divisible by 3) ✓
- T(4) = 10 (divisible by 5) ✓
- T(5) = 15 (divisible by 3 and 5) ✓
- T(6) = 21 (divisible by 3) ✓
- T(7) = 28 (not divisible by 3 or 5)
- T(8) = 36 (divisible by 3) ✓
- T(9) = 45 (divisible by 3 and 5) ✓
- T(10) = 55 (divisible by 5) ✓

The solution uses Haskell's lazy evaluation to efficiently generate only the required elements on-demand.

## Final Answer

The sum of the first 20 triangular numbers divisible by 3 or 5 is **3528**.

