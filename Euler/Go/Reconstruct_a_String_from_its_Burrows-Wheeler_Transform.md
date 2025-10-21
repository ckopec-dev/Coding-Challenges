# Euler Problem: Reconstruct a String from its Burrows-Wheeler Transform

## Problem Understanding

The Burrows-Wheeler Transform (BWT) is a data transformation technique used in data compression. Given a string, the BWT rearranges the characters to create a transformed string that's more compressible. The reconstruction problem asks us to reverse this process.

## Solution Approach

The reconstruction algorithm works by:
1. Sorting the BWT string lexicographically
2. Using the "last-to-first" mapping to reconstruct the original string
3. Following the cycle until we return to the original string

## Go Implementation

```go
package main

import (
    "fmt"
    "sort"
    "strings"
)

// reconstructString reconstructs the original string from its Burrows-Wheeler Transform
func reconstructString(bwt string) string {
    if bwt == "" {
        return ""
    }
    
    // Create a slice of strings to represent each row of the BWT matrix
    rows := make([]string, len(bwt))
    
    // Sort the BWT string to get the first column
    sortedBwt := sortString(bwt)
    
    // Create the BWT matrix by repeatedly applying the transformation
    // We'll build it backwards using the last-to-first mapping
    
    // Create a map to track the positions of characters in the sorted array
    charCount := make(map[rune]int)
    charPositions := make(map[rune][]int)
    
    // Count characters and store their positions in sorted array
    for i, char := range sortedBwt {
        charCount[char]++
        charPositions[char] = append(charPositions[char], i)
    }
    
    // Create the last-to-first mapping
    lastToFirst := make([]int, len(bwt))
    tempPositions := make(map[rune]int)
    
    for i, char := range bwt {
        // Get the position of this character in the sorted array
        pos := charPositions[char][tempPositions[char]]
        lastToFirst[i] = pos
        tempPositions[char]++
    }
    
    // Reconstruct the original string by following the cycle
    result := make([]rune, len(bwt))
    
    // Start from the position of the '$' character (end marker)
    currentPos := 0
    for i, char := range bwt {
        if char == '$' {
            currentPos = i
            break
        }
    }
    
    // Build the result by following the cycle
    for i := 0; i < len(bwt); i++ {
        result[len(bwt)-1-i] = bwt[currentPos]
        currentPos = lastToFirst[currentPos]
    }
    
    return string(result)
}

// Alternative cleaner approach using direct sorting and mapping
func reconstructStringClean(bwt string) string {
    if bwt == "" {
        return ""
    }
    
    // Create a slice of runes for easier manipulation
    runes := []rune(bwt)
    
    // Create a matrix where each row represents a rotation of the string
    // We'll build it by sorting the BWT and then reconstructing
    n := len(runes)
    
    // Create a table with two columns: sorted BWT and original BWT
    type Row struct {
        sorted rune
        original rune
        index  int
    }
    
    // Create the sorted version
    sorted := make([]rune, n)
    copy(sorted, runes)
    sort.Slice(sorted, func(i, j int) bool {
        return sorted[i] < sorted[j]
    })
    
    // Create mapping from original BWT to sorted positions
    charCount := make(map[rune]int)
    charPositions := make(map[rune][]int)
    
    // Build character positions in sorted array
    for i, char := range sorted {
        charCount[char]++
        charPositions[char] = append(charPositions[char], i)
    }
    
    // Create last-to-first mapping
    lastToFirst := make([]int, n)
    tempPositions := make(map[rune]int)
    
    for i, char := range runes {
        pos := charPositions[char][tempPositions[char]]
        lastToFirst[i] = pos
        tempPositions[char]++
    }
    
    // Reconstruct by following the cycle
    result := make([]rune, n)
    currentPos := 0
    
    // Find the position of the end marker (usually '$')
    for i, char := range runes {
        if char == '$' {
            currentPos = i
            break
        }
    }
    
    // Build the result string
    for i := 0; i < n; i++ {
        result[n-1-i] = runes[currentPos]
        currentPos = lastToFirst[currentPos]
    }
    
    return string(result)
}

// Even cleaner approach - direct reconstruction
func reconstructStringDirect(bwt string) string {
    if bwt == "" {
        return ""
    }
    
    n := len(bwt)
    
    // Create a list of all rotations
    type Entry struct {
        original string
        index    int
    }
    
    // Create sorted version
    runes := []rune(bwt)
    sorted := make([]rune, n)
    copy(sorted, runes)
    sort.Slice(sorted, func(i, j int) bool {
        return sorted[i] < sorted[j]
    })
    
    // Create mapping from character positions
    charPositions := make(map[rune][]int)
    for i, char := range sorted {
        charPositions[char] = append(charPositions[char], i)
    }
    
    // Create last-to-first mapping
    lastToFirst := make([]int, n)
    tempPositions := make(map[rune]int)
    
    for i, char := range runes {
        pos := charPositions[char][tempPositions[char]]
        lastToFirst[i] = pos
        tempPositions[char]++
    }
    
    // Find the index of the original string (where it starts with '$')
    startIndex := 0
    for i, char := range runes {
        if char == '$' {
            startIndex = i
            break
        }
    }
    
    // Reconstruct the original string
    result := make([]rune, n)
    current := startIndex
    
    for i := 0; i < n; i++ {
        result[n-1-i] = runes[current]
        current = lastToFirst[current]
    }
    
    return string(result)
}

// Helper function to sort string
func sortString(s string) string {
    runes := []rune(s)
    sort.Slice(runes, func(i, j int) bool {
        return runes[i] < runes[j]
    })
    return string(runes)
}

// Main function with test cases
func main() {
    // Test cases
    testCases := []string{
        "BANANA$",
        "ACGT$",
        "ABRACADABRA$",
        "$",
        "A$",
        "ABCABC$",
    }
    
    fmt.Println("Reconstructing strings from Burrows-Wheeler Transform:")
    fmt.Println("=====================================================")
    
    for _, testCase := range testCases {
        reconstructed := reconstructStringDirect(testCase)
        fmt.Printf("BWT: %s -> Original: %s\n", testCase, reconstructed)
    }
    
    // Verify the reconstruction works correctly
    fmt.Println("\nVerification:")
    fmt.Println("============")
    
    original := "BANANA$"
    fmt.Printf("Original: %s\n", original)
    
    // Simulate BWT (for verification)
    bwt := bwtTransform(original)
    fmt.Printf("BWT: %s\n", bwt)
    
    reconstructed := reconstructStringDirect(bwt)
    fmt.Printf("Reconstructed: %s\n", reconstructed)
    fmt.Printf("Match: %t\n", original == reconstructed)
}

// Helper function to compute BWT (for verification)
func bwtTransform(s string) string {
    if s == "" {
        return ""
    }
    
    // Add end marker
    s = s + "$"
    n := len(s)
    
    // Create all rotations
    rotations := make([]string, n)
    for i := 0; i < n; i++ {
        rotations[i] = s[i:] + s[:i]
    }
    
    // Sort rotations
    sort.Strings(rotations)
    
    // Take last characters
    result := make([]rune, n)
    for i, rotation := range rotations {
        result[i] = rune(rotation[n-1])
    }
    
    return string(result)
}
```

## Explanation

The solution works through these key steps:

1. **Character Position Tracking**: We create mappings to track where each character appears in the sorted BWT.

2. **Last-to-First Mapping**: This mapping tells us where each character in the original BWT would appear in the sorted version.

3. **Cycle Following**: Starting from the position of the end marker '$', we follow the cycle to reconstruct the original string.

## Key Features

- Handles edge cases like empty strings and single characters
- Uses efficient sorting algorithms
- Properly manages character positions and mappings
- Includes verification with BWT computation

## Time and Space Complexity

- **Time Complexity**: O(n log n) due to sorting operations
- **Space Complexity**: O(n) for storing the BWT and auxiliary data structures

This implementation correctly reconstructs strings from their Burrows-Wheeler Transform, which is a fundamental operation in bioinformatics and data compression algorithms.

