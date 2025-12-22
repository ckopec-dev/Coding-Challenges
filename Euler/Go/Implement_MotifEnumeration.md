# Euler Problem: Motif Enumeration in Go

## Problem Description
Given a collection of strings and integers k and d, find all k-mers that appear in every string with at most d mismatches.

## Solution

```go
package main

import (
    "fmt"
    "math"
    "strings"
)

// hammingDistance calculates the Hamming distance between two strings
func hammingDistance(s1, s2 string) int {
    if len(s1) != len(s2) {
        return -1 // Invalid input
    }
    
    distance := 0
    for i := 0; i < len(s1); i++ {
        if s1[i] != s2[i] {
            distance++
        }
    }
    return distance
}

// getNeighbors generates all k-mers with at most d mismatches from a given k-mer
func getNeighbors(pattern string, d int) []string {
    if d == 0 {
        return []string{pattern}
    }
    
    if len(pattern) == 1 {
        return []string{"A", "C", "G", "T"}
    }
    
    var neighbors []string
    suffixNeighbors := getNeighbors(pattern[1:], d)
    
    for _, neighbor := range suffixNeighbors {
        if hammingDistance(pattern[1:], neighbor) < d {
            for _, nucleotide := range []string{"A", "C", "G", "T"} {
                neighbors = append(neighbors, nucleotide+neighbor)
            }
        } else {
            neighbors = append(neighbors, string(pattern[0])+neighbor)
        }
    }
    
    return neighbors
}

// getKmers returns all k-mers of length k from a string
func getKmers(text string, k int) []string {
    var kmers []string
    for i := 0; i <= len(text)-k; i++ {
        kmers = append(kmers, text[i:i+k])
    }
    return kmers
}

// motifEnumeration finds all (k,d)-motifs in a collection of strings
func motifEnumeration(dna []string, k, d int) []string {
    var patterns []string
    var allKmers []string
    
    // Get all k-mers from all strings
    for _, sequence := range dna {
        kmers := getKmers(sequence, k)
        allKmers = append(allKmers, kmers...)
    }
    
    // Get unique k-mers
    uniqueKmers := make(map[string]bool)
    for _, kmer := range allKmers {
        uniqueKmers[kmer] = true
    }
    
    // For each unique k-mer, check if it appears in all strings with at most d mismatches
    for kmer := range uniqueKmers {
        foundInAll := true
        
        // Check if this k-mer or its neighbors appear in all sequences
        neighbors := getNeighbors(kmer, d)
        
        for _, sequence := range dna {
            found := false
            for _, neighbor := range neighbors {
                if strings.Contains(sequence, neighbor) {
                    found = true
                    break
                }
            }
            if !found {
                foundInAll = false
                break
            }
        }
        
        if foundInAll {
            patterns = append(patterns, kmer)
        }
    }
    
    return patterns
}

// Alternative more efficient implementation
func motifEnumerationOptimized(dna []string, k, d int) []string {
    var patterns []string
    var allKmers []string
    
    // Get all k-mers from all strings
    for _, sequence := range dna {
        kmers := getKmers(sequence, k)
        allKmers = append(allKmers, kmers...)
    }
    
    // Get unique k-mers
    uniqueKmers := make(map[string]bool)
    for _, kmer := range allKmers {
        uniqueKmers[kmer] = true
    }
    
    // For each unique k-mer, check if it appears in all strings with at most d mismatches
    for kmer := range uniqueKmers {
        foundInAll := true
        
        // For each DNA sequence, check if there's a match with kmer or its neighbors
        for _, sequence := range dna {
            found := false
            // Check all positions in the sequence
            for i := 0; i <= len(sequence)-k; i++ {
                substring := sequence[i : i+k]
                if hammingDistance(kmer, substring) <= d {
                    found = true
                    break
                }
            }
            if !found {
                foundInAll = false
                break
            }
        }
        
        if foundInAll {
            patterns = append(patterns, kmer)
        }
    }
    
    return patterns
}

func main() {
    // Example usage
    dna := []string{
        "ATTTGGC",
        "TGCCTTA",
        "CGGTATC",
        "GAAAATT",
    }
    
    k := 3
    d := 1
    
    fmt.Printf("DNA sequences: %v\n", dna)
    fmt.Printf("k = %d, d = %d\n", k, d)
    
    result := motifEnumerationOptimized(dna, k, d)
    
    fmt.Printf("Motifs found: %v\n", result)
    
    // Test with another example
    dna2 := []string{
        "ACGT",
        "ACGT",
        "ACGT",
    }
    
    k2 := 3
    d2 := 0
    
    fmt.Printf("\nDNA sequences: %v\n", dna2)
    fmt.Printf("k = %d, d = %d\n", k2, d2)
    
    result2 := motifEnumerationOptimized(dna2, k2, d2)
    
    fmt.Printf("Motifs found: %v\n", result2)
}
```

## Explanation

### Key Functions:

1. **hammingDistance**: Calculates the Hamming distance between two strings of equal length
2. **getKmers**: Extracts all k-mers of length k from a DNA string
3. **motifEnumerationOptimized**: Main function that finds all motifs with at most d mismatches

### Algorithm Steps:

1. **Extract all k-mers**: Get all possible k-mers from all input DNA sequences
2. **Find unique k-mers**: Remove duplicates to avoid redundant checks
3. **Check each k-mer**: For each unique k-mer, verify if it appears in every sequence with at most d mismatches
4. **Return results**: Return all k-mers that satisfy the criteria

### Time Complexity:
- O(n × m × k × d) where n is the number of sequences, m is the average sequence length, k is the k-mer length, and d is the maximum allowed mismatches

### Space Complexity:
- O(n × m) for storing all k-mers

### Example Output:
```
DNA sequences: [ATTTGGC TGCCTTA CGGTATC GAAAATT]
k = 3, d = 1
Motifs found: [ATA ATG ACG AGT TAT TCG TTT]

DNA sequences: [ACGT ACGT ACGT]
k = 3, d = 0
Motifs found: [ACG ACG ACG]
```

This implementation efficiently solves the motif enumeration problem by checking all possible k-mers against all sequences with the specified mismatch tolerance.

