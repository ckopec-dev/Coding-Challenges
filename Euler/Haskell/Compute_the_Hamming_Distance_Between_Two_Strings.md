# Euler Problem: Hamming Distance Between Two Strings

The Hamming distance between two strings of equal length is the number of positions at which the corresponding characters are different.

## Solution in Haskell

```haskell
-- Compute the Hamming distance between two strings
hammingDistance :: String -> String -> Int
hammingDistance xs ys = sum $ zipWith (\x y -> if x == y then 0 else 1) xs ys

-- Alternative implementation using zipWith and abs
hammingDistance' :: String -> String -> Int
hammingDistance' xs ys = sum $ map (fromEnum . notEqual) $ zip xs ys
  where
    notEqual (x, y) = x /= y

-- More concise version
hammingDistance'' :: String -> String -> Int
hammingDistance'' = sum . map (fromEnum . uncurry (/=)) . zip

-- Using foldl for a more explicit approach
hammingDistance''' :: String -> String -> Int
hammingDistance''' xs ys = foldl (\acc (x, y) -> acc + if x == y then 0 else 1) 0 $ zip xs ys
```

## Example Usage

```haskell
-- Example usage
main :: IO ()
main = do
    let str1 = "GAGCCTACTAACGGGAT"
    let str2 = "CATCGGTATCGCCCGTT"
    
    print $ hammingDistance str1 str2  -- Output: 7
    
    -- Additional test cases
    print $ hammingDistance "abc" "abc"  -- Output: 0
    print $ hammingDistance "abc" "def"  -- Output: 3
    print $ hammingDistance "abc" "abd"  -- Output: 1
```

## Explanation

The solution works by:

1. **Zipping the strings together** - `zip xs ys` creates pairs of corresponding characters
2. **Comparing each pair** - For each pair `(x, y)`, we check if `x /= y`
3. **Converting boolean to integer** - `fromEnum` converts `True` to `1` and `False` to `0`
4. **Summing the results** - `sum` adds up all the differences

## Key Haskell Concepts Used

- **Higher-order functions**: `zipWith`, `map`, `foldl`
- **Function composition**: Using `.` to compose functions
- **Pattern matching**: Direct pattern matching in lambda functions
- **Type inference**: Haskell automatically infers the types
- **Lazy evaluation**: The computation is only performed when needed

## Alternative Approach with Error Handling

```haskell
-- Version with length checking
hammingDistanceSafe :: String -> String -> Either String Int
hammingDistanceSafe xs ys
    | length xs /= length ys = Left "Strings must be of equal length"
    | otherwise = Right $ sum $ zipWith (\x y -> if x == y then 0 else 1) xs ys
```

This solution efficiently computes the Hamming distance in O(n) time complexity where n is the length of the strings.

