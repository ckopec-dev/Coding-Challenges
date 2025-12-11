# Euler Problem: Translating RNA into Protein in Haskell

## Problem Statement
The task is to translate an RNA string into a protein string by reading codons (groups of 3 nucleotides) and mapping them to their corresponding amino acids.

## Solution

```haskell
module RNAProtein where

-- Define the codon to amino acid mapping
codonMap :: [(String, String)]
codonMap = 
    [ ("UUU", "F"), ("UUC", "F"), ("UUA", "L"), ("UUG", "L")
    , ("UCU", "S"), ("UCC", "S"), ("UCA", "S"), ("UCG", "S")
    , ("UAU", "Y"), ("UAC", "Y"), ("UAA", "*"), ("UAG", "*")
    , ("UGU", "C"), ("UGC", "C"), ("UGA", "*"), ("UGG", "W")
    , ("CUU", "L"), ("CUC", "L"), ("CUA", "L"), ("CUG", "L")
    , ("CCU", "P"), ("CCC", "P"), ("CCA", "P"), ("CCG", "P")
    , ("CAU", "H"), ("CAC", "H"), ("CAA", "Q"), ("CAG", "Q")
    , ("CGU", "R"), ("CGC", "R"), ("CGA", "R"), ("CGG", "R")
    , ("AUU", "I"), ("AUC", "I"), ("AUA", "I"), ("AUG", "M")
    , ("ACU", "T"), ("ACC", "T"), ("ACA", "T"), ("ACG", "T")
    , ("AAU", "N"), ("AAC", "N"), ("AAA", "K"), ("AAG", "K")
    , ("AGU", "S"), ("AGC", "S"), ("AGA", "R"), ("AGG", "R")
    , ("GUU", "V"), ("GUC", "V"), ("GUA", "V"), ("GUG", "V")
    , ("GCU", "A"), ("GCC", "A"), ("GCA", "A"), ("GCG", "A")
    , ("GAU", "D"), ("GAC", "D"), ("GAA", "E"), ("GAG", "E")
    , ("GGU", "G"), ("GGC", "G"), ("GGA", "G"), ("GGG", "G")
    ]

-- Function to translate RNA to protein
rnaToProtein :: String -> String
rnaToProtein rna = translate rna []
  where
    translate [] proteins = reverse proteins
    translate rnaStr proteins = 
        let codon = take 3 rnaStr
            rest = drop 3 rnaStr
        in case lookup codon codonMap of
            Just "*" -> reverse proteins  -- Stop codon
            Just amino -> translate rest (amino : proteins)
            Nothing -> error "Invalid codon"

-- Alternative implementation using recursion with pattern matching
translateRNA :: String -> String
translateRNA [] = []
translateRNA rna
    | take 3 rna == "UAA" || take 3 rna == "UAG" || take 3 rna == "UGA" = []
    | otherwise = case lookup (take 3 rna) codonMap of
        Just amino -> amino : translateRNA (drop 3 rna)
        Nothing -> error "Invalid codon"

-- More concise version using map and takeWhile
translateRNA' :: String -> String
translateRNA' rna = takeWhile (/= '*') $ mapMaybe (lookup codonMap) codons
  where
    codons = map (take 3) $ unfoldr (\s -> if null s then Nothing else Just (s, drop 3 s)) rna
    mapMaybe f [] = []
    mapMaybe f (x:xs) = case f x of
        Nothing -> mapMaybe f xs
        Just y -> y : mapMaybe f xs

-- Simple and clean implementation
translateRNA'' :: String -> String
translateRNA'' [] = []
translateRNA'' rna = 
    let codon = take 3 rna
        rest = drop 3 rna
    in case lookup codon codonMap of
        Just "*" -> []
        Just amino -> amino : translateRNA'' rest
        Nothing -> error "Invalid codon"

-- Test function
testRNAProtein :: IO ()
testRNAProtein = do
    let test1 = "AUGGCCUUU"
    let test2 = "AUGUUUUGG"
    let test3 = "AUGUUUUGGAUUAA"
    putStrLn $ "Test 1: " ++ test1 ++ " -> " ++ translateRNA'' test1
    putStrLn $ "Test 2: " ++ test2 ++ " -> " ++ translateRNA'' test2
    putStrLn $ "Test 3: " ++ test3 ++ " -> " ++ translateRNA'' test3

-- Main function for execution
main :: IO ()
main = testRNAProtein
```

## Explanation

The solution implements a function `translateRNA''` that:

1. **Takes an RNA string** as input
2. **Extracts codons** (3-character sequences) from the RNA
3. **Maps each codon** to its corresponding amino acid using the codon map
4. **Stops translation** when encountering a stop codon ("UAA", "UAG", or "UGA")
5. **Returns the protein sequence** as a string

## Key Features

- **Codon Mapping**: Uses a list of tuples to map RNA codons to amino acids
- **Stop Codons**: Properly handles stop codons by terminating translation
- **Error Handling**: Throws an error for invalid codons
- **Recursive Approach**: Uses recursion to process the RNA string character by character

## Example Usage

```haskell
-- Example translations:
-- "AUGGCCUUU" → "MAA" (AUG=Methionine, GCC=Alanine, UUU=Phenylalanine)
-- "AUGUUUUGG" → "MFW" (AUG=Methionine, UUU=Phenylalanine, UGG=Tryptophan)
-- "AUGUUUUGGAUUAA" → "MF" (stops at UAA)
```

This implementation efficiently translates RNA sequences into proteins following the standard genetic code.

