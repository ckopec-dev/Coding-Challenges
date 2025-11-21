# Finding the Shortest Non-Shared Substring of Two Strings in Haskell

## Problem Understanding

Given two strings, we need to find the shortest substring that appears in one string but not in the other.

## Solution Approach

We'll use a systematic approach:
1. Generate all possible substrings of both strings
2. Find substrings that are unique to each string
3. Return the shortest such substring

## Haskell Implementation

```haskell
import Data.List (sort)
import Data.Ord (comparing)

-- Find all substrings of a string
substrings :: String -> [String]
substrings [] = []
substrings s = [take n (drop i s) | i <- [0..length s - 1], n <- [1..length s - i]]

-- Find shortest non-shared substring
shortestNonSharedSubstring :: String -> String -> String
shortestNonSharedSubstring s1 s2
  | null s1 && null s2 = ""
  | null s1 = head $ sortOn length $ filter (not . (`elem` s1Subs)) s2Subs
  | null s2 = head $ sortOn length $ filter (not . (`elem` s2Subs)) s1Subs
  | otherwise = 
      let s1Subs = substrings s1
          s2Subs = substrings s2
          uniqueToS1 = filter (not . (`elem` s2Subs)) s1Subs
          uniqueToS2 = filter (not . (`elem` s1Subs)) s2Subs
          allUnique = uniqueToS1 ++ uniqueToS2
      in if null allUnique 
         then "" 
         else head $ sortOn length allUnique

-- Alternative more efficient approach
shortestNonSharedSubstring' :: String -> String -> String
shortestNonSharedSubstring' s1 s2 = 
  let s1Subs = substrings s1
      s2Subs = substrings s2
      s1Unique = filter (not . (`elem` s2Subs)) s1Subs
      s2Unique = filter (not . (`elem` s1Subs)) s2Subs
      allUnique = s1Unique ++ s2Unique
  in if null allUnique 
     then "" 
     else head $ sortOn length allUnique

-- More efficient version using set operations
import qualified Data.Set as Set

shortestNonSharedSubstring'' :: String -> String -> String
shortestNonSharedSubstring'' s1 s2 = 
  let s1Subs = Set.fromList $ substrings s1
      s2Subs = Set.fromList $ substrings s2
      s1Unique = filter (not . (`Set.member` s2Subs)) $ substrings s1
      s2Unique = filter (not . (`Set.member` s1Subs)) $ substrings s2
      allUnique = s1Unique ++ s2Unique
  in if null allUnique 
     then "" 
     else head $ sortOn length allUnique

-- Helper function to sort by length
sortOnLength :: [String] -> [String]
sortOnLength = sortOn length

-- Test examples
main :: IO ()
main = do
  putStrLn "Testing shortest non-shared substring:"
  putStrLn $ "s1=\"abc\", s2=\"def\" -> " ++ shortestNonSharedSubstring "abc" "def"
  putStrLn $ "s1=\"abc\", s2=\"bcd\" -> " ++ shortestNonSharedSubstring "abc" "bcd"
  putStrLn $ "s1=\"hello\", s2=\"world\" -> " ++ shortestNonSharedSubstring "hello" "world"
  putStrLn $ "s1=\"ab\", s2=\"ab\" -> " ++ shortestNonSharedSubstring "ab" "ab"
```

## Example Usage

```haskell
-- Examples:
-- shortestNonSharedSubstring "abc" "def"  -- Returns "a" (or any single char)
-- shortestNonSharedSubstring "abc" "bcd"  -- Returns "a" (unique to first string)
-- shortestNonSharedSubstring "hello" "world" -- Returns "h" (unique to first string)
-- shortestNonSharedSubstring "ab" "ab" -- Returns "" (no unique substrings)
```

## Time Complexity Analysis

- **Time Complexity**: O(n³ + m³) where n and m are the lengths of the strings
- **Space Complexity**: O(n³ + m³) for storing all substrings

## Key Features

1. **Handles edge cases**: Empty strings, identical strings
2. **Efficient search**: Finds the shortest unique substring
3. **Clear logic**: Separates unique substrings from shared ones
4. **Type-safe**: Uses Haskell's strong typing system

## Optimized Version

For better performance with large inputs:

```haskell
import qualified Data.Set as Set

shortestNonSharedSubstringOptimized :: String -> String -> String
shortestNonSharedSubstringOptimized s1 s2
  | s1 == s2 = ""
  | otherwise = 
      let s1Subs = Set.fromList $ substrings s1
          s2Subs = Set.fromList $ substrings s2
          candidates = filter (not . (`Set.member` s2Subs)) (substrings s1) 
                    ++ filter (not . (`Set.member` s1Subs)) (substrings s2)
      in if null candidates 
         then "" 
         else head $ sortOn length candidates
```

This solution efficiently finds the shortest substring that exists in one string but not in the other, using Haskell's functional programming features and efficient set operations.

