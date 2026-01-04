# Euler Problem: Burrows-Wheeler Transform in Haskell

The Burrows-Wheeler Transform (BWT) is a data transformation algorithm that rearranges a string into a form that is more amenable to compression. Here's a Haskell implementation:

```haskell
-- Function to compute all rotations of a string
rotations :: String -> [String]
rotations [] = []
rotations s = take (length s) $ map (drop n) $ iterate (rotateRight 1) s
  where
    rotateRight n xs = drop n xs ++ take n xs

-- Function to compute the Burrows-Wheeler Transform
bwt :: String -> String
bwt s = map last $ sort $ rotations s
  where
    sort = sortBy compare

-- Alternative implementation using explicit sorting
bwt' :: String -> String
bwt' s = map last $ sortWith last $ rotations s
  where
    sortWith f = sortBy (compare `on` f)

-- More efficient implementation using indices
bwtEfficient :: String -> String
bwtEfficient s = map (s !!) $ map (subtract 1) $ sort $ map (index s) [0..length s - 1]
  where
    index str i = (str ++ str) !! i
    sort = sortBy compare

-- Simple and clean implementation
bwtSimple :: String -> String
bwtSimple s = map last $ sort $ map (take (length s)) $ iterate (rotateRight 1) s
  where
    rotateRight n xs = drop n xs ++ take n xs

-- Correct implementation using proper rotation
bwtCorrect :: String -> String
bwtCorrect s = map last $ sort $ rotations s
  where
    rotations [] = []
    rotations str = let n = length str in 
                    map (take n . drop (n - i)) [0..n-1]
                    where
                      rotateRight i xs = drop i xs ++ take i xs

-- Final clean implementation
bwt :: String -> String
bwt s = map last $ sort $ map (take n) $ map (drop (n - i)) [0..n-1]
  where
    n = length s
    sort = sortBy compare
```

## Correct Solution

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)

-- Burrows-Wheeler Transform implementation
bwt :: String -> String
bwt s = map last $ sortBy compare $ rotations s
  where
    rotations [] = []
    rotations str = let n = length str in 
                    [take n $ drop (n - i) $ str ++ str | i <- [0..n-1]]

-- Alternative clean version
bwtClean :: String -> String
bwtClean s = map last $ sort $ map (take (length s)) $ map (rotateRight) [0..length s - 1]
  where
    rotateRight n xs = drop n xs ++ take n xs
    sort = sortBy compare

-- Most straightforward implementation
bwtFinal :: String -> String
bwtFinal s = map last $ sortBy compare $ allRotations s
  where
    allRotations [] = []
    allRotations str = let n = length str in 
                       [rotate n i str | i <- [0..n-1]]
    rotate n i str = take n $ drop (n - i) $ str ++ str
```

## Usage Example

```haskell
-- Example usage
main :: IO ()
main = do
    let text = "banana"
    let result = bwt text
    putStrLn $ "Original: " ++ text
    putStrLn $ "BWT:      " ++ result
    
    -- Test with another example
    let text2 = "abracadabra"
    let result2 = bwt text2
    putStrLn $ "Original: " ++ text2
    putStrLn $ "BWT:      " ++ result2
```

## Explanation

The Burrows-Wheeler Transform works by:

1. **Generating all rotations** of the input string
2. **Sorting all rotations** lexicographically
3. **Taking the last character** of each sorted rotation

For example, with "banana":
1. Rotations: ["banana", "nabana", "anaban", "nabana", "anaban", "nabana"]
2. Sorted: ["anaban", "anaban", "banana", "nabana", "nabana", "nabana"]
3. Last characters: "nabnnn"

The BWT is useful for data compression because it tends to group similar characters together, making the transformed data more compressible.

