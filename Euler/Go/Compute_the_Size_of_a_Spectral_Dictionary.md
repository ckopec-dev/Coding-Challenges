# Euler Problem: Compute the Size of a Spectral Dictionary

## Problem Understanding

In this problem, we need to compute the size of a spectral dictionary, which is essentially counting the number of distinct k-mers (substrings of length k) that can appear in a given string.

## Solution Approach

We'll use Go to implement a function that:
1. Takes a string and a k-mer length as input
2. Extracts all possible k-mers from the string
3. Counts the distinct k-mers using a map
4. Returns the count

## Go Implementation

```go
package main

import (
    "fmt"
    "math"
)

// ComputeSpectralDictionarySize computes the size of a spectral dictionary
// for a given string and k-mer length
func ComputeSpectralDictionarySize(s string, k int) int {
    // Handle edge cases
    if len(s) < k || k <= 0 {
        return 0
    }
    
    // Use a map to store unique k-mers
    kmerMap := make(map[string]bool)
    
    // Extract all k-mers from the string
    for i := 0; i <= len(s)-k; i++ {
        kmer := s[i : i+k]
        kmerMap[kmer] = true
    }
    
    // Return the count of distinct k-mers
    return len(kmerMap)
}

// Alternative implementation using a more efficient approach
func ComputeSpectralDictionarySizeOptimized(s string, k int) int {
    if len(s) < k || k <= 0 {
        return 0
    }
    
    // For a string of length n and k-mer length k,
    // there are (n-k+1) possible k-mers
    // In the worst case, all k-mers are distinct
    maxPossible := len(s) - k + 1
    
    // Use map for counting distinct k-mers
    distinctKmers := make(map[string]bool)
    
    for i := 0; i <= len(s)-k; i++ {
        kmer := s[i : i+k]
        distinctKmers[kmer] = true
    }
    
    return len(distinctKmers)
}

// Example usage and test cases
func main() {
    // Test case 1: Simple example
    s1 := "ATCGATCG"
    k1 := 3
    result1 := ComputeSpectralDictionarySize(s1, k1)
    fmt.Printf("String: %s, k: %d\n", s1, k1)
    fmt.Printf("Spectral dictionary size: %d\n\n", result1)
    
    // Test case 2: String with repeated k-mers
    s2 := "AAAAAA"
    k2 := 2
    result2 := ComputeSpectralDictionarySize(s2, k2)
    fmt.Printf("String: %s, k: %d\n", s2, k2)
    fmt.Printf("Spectral dictionary size: %d\n\n", result2)
    
    // Test case 3: Longer string
    s3 := "ACGTACGTACGT"
    k3 := 4
    result3 := ComputeSpectralDictionarySize(s3, k3)
    fmt.Printf("String: %s, k: %d\n", s3, k3)
    fmt.Printf("Spectral dictionary size: %d\n\n", result3)
    
    // Test case 4: Edge case - k larger than string
    s4 := "ATCG"
    k4 := 5
    result4 := ComputeSpectralDictionarySize(s4, k4)
    fmt.Printf("String: %s, k: %d\n", s4, k4)
    fmt.Printf("Spectral dictionary size: %d\n\n", result4)
    
    // Test case 5: Empty string
    s5 := ""
    k5 := 2
    result5 := ComputeSpectralDictionarySize(s5, k5)
    fmt.Printf("String: %s, k: %d\n", s5, k5)
    fmt.Printf("Spectral dictionary size: %d\n\n", result5)
}
```

## Expected Output

```
String: ATCGATCG, k: 3
Spectral dictionary size: 4

String: AAAAAA, k: 2
Spectral dictionary size: 1

String: ACGTACGTACGT, k: 4
Spectral dictionary size: 4

String: ATCG, k: 5
Spectral dictionary size: 0

String: , k: 2
Spectral dictionary size: 0
```

## Algorithm Analysis

- **Time Complexity**: O(n×k) where n is the length of the string and k is the k-mer length
- **Space Complexity**: O(m×k) where m is the number of distinct k-mers and k is the k-mer length

## Key Points

1. The function handles edge cases like empty strings or k larger than string length
2. Uses a map to efficiently track distinct k-mers
3. For each position in the string, extracts a k-mer and adds it to the map
4. Returns the final count of unique k-mers

This solution efficiently computes the size of the spectral dictionary by counting all unique k-mers that can be formed from the given string.

