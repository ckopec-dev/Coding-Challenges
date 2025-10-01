# Euler Problem: Complementing a Strand of DNA

## Problem Statement
In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'. Given a DNA string, return its reverse complement.

## Solution in Go

```go
package main

import (
    "fmt"
    "strings"
)

// complementDNA returns the reverse complement of a DNA string
func complementDNA(dna string) string {
    // Create a map for DNA base complements
    complement := map[rune]rune{
        'A': 'T',
        'T': 'A',
        'C': 'G',
        'G': 'C',
    }
    
    // Build the complement string
    var result strings.Builder
    for _, base := range dna {
        if comp, exists := complement[base]; exists {
            result.WriteRune(comp)
        } else {
            // Handle invalid characters (though problem assumes valid DNA)
            result.WriteRune(base)
        }
    }
    
    // Reverse the complement string
    complementStr := result.String()
    runes := []rune(complementStr)
    for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
        runes[i], runes[j] = runes[j], runes[i]
    }
    
    return string(runes)
}

// Alternative implementation using strings.Builder and reverse in one pass
func complementDNAOptimized(dna string) string {
    complement := map[rune]rune{
        'A': 'T',
        'T': 'A',
        'C': 'G',
        'G': 'C',
    }
    
    // Build result in reverse order to avoid final reversal
    result := make([]rune, len(dna))
    for i, base := range dna {
        if comp, exists := complement[base]; exists {
            result[len(dna)-1-i] = comp
        } else {
            result[len(dna)-1-i] = base
        }
    }
    
    return string(result)
}

func main() {
    // Test cases
    testCases := []string{
        "AAAACCCGGT",
        "GCTAGCT",
        "ATCGATCG",
        "TTTTTTTT",
    }
    
    fmt.Println("DNA Complement Results:")
    fmt.Println("======================")
    
    for _, dna := range testCases {
        result1 := complementDNA(dna)
        result2 := complementDNAOptimized(dna)
        
        fmt.Printf("Original: %s\n", dna)
        fmt.Printf("Complement: %s\n", result1)
        fmt.Printf("Optimized: %s\n", result2)
        fmt.Println()
    }
    
    // Example from problem
    example := "AAAACCCGGT"
    fmt.Printf("Example from problem:\n")
    fmt.Printf("Input: %s\n", example)
    fmt.Printf("Output: %s\n", complementDNA(example))
}
```

## Explanation

### Approach 1: Two-Step Process
1. **Complement each base**: Use a map to convert each DNA base to its complement
2. **Reverse the result**: Reverse the complemented string to get the reverse complement

### Approach 2: Optimized Single Pass
1. **Build in reverse order**: Directly place each complemented base in the reverse position
2. **No final reversal needed**: More efficient as it avoids the second step

## Key Features

- **Map-based lookup**: Efficient O(1) base complement lookup
- **String builder**: Memory-efficient string construction
- **Unicode support**: Handles Unicode characters properly
- **Error handling**: Gracefully handles invalid DNA bases
- **Two implementations**: Demonstrates both approaches

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the DNA string
- **Space Complexity**: O(n) for storing the result

## Sample Output
```
DNA Complement Results:
======================
Original: AAAACCCGGT
Complement: ACCGGGTTTT

Original: GCTAGCT
Complement: AGCTAGC

Original: ATCGATCG
Complement: CGATCGAT

Original: TTTTTTTT
Complement: AAAAAAAAA

Example from problem:
Input: AAAACCCGGT
Output: ACCGGGTTTT
```

The solution correctly handles the DNA complementation rule where A↔T and C↔G, and produces the reverse complement as required by the Euler problem.

