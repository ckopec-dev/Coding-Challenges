# Finding a Middle Edge in an Alignment Graph in Linear Space

## Problem Understanding

In bioinformatics, we often need to find optimal alignments between sequences. The middle edge in an alignment graph is crucial for implementing the divide-and-conquer approach to sequence alignment in linear space.

## Solution Approach

We'll implement a solution that finds the middle edge in an alignment graph using linear space by:
1. Computing the score of the middle column
2. Finding the middle edge that maximizes the alignment score
3. Using dynamic programming with space optimization

## Go Implementation

```go
package main

import (
    "fmt"
    "math"
)

// MiddleEdge represents an edge in the alignment graph
type MiddleEdge struct {
    FromRow, FromCol int
    ToRow, ToCol     int
    Score            int
}

// findMiddleEdge finds the middle edge in an alignment graph
func findMiddleEdge(s, t string, sigma int, delta int) MiddleEdge {
    m := len(s)
    n := len(t)
    
    // Middle column index
    midCol := n / 2
    
    // Compute scores for middle column using linear space
    scores := make([]int, m+1)
    
    // Initialize first row
    for i := 0; i <= m; i++ {
        scores[i] = i * delta
    }
    
    // Fill the middle column
    for j := 1; j <= midCol; j++ {
        prevScores := make([]int, m+1)
        copy(prevScores, scores)
        
        scores[0] = j * delta
        for i := 1; i <= m; i++ {
            match := prevScores[i-1]
            if s[i-1] == t[j-1] {
                match += sigma
            } else {
                match -= sigma
            }
            
            delete := prevScores[i] - delta
            insert := scores[i-1] - delta
            
            scores[i] = max(match, delete, insert)
        }
    }
    
    // Find the middle edge
    maxScore := math.MinInt32
    var middleEdge MiddleEdge
    
    // Check vertical edges (from row i to i+1)
    for i := 0; i < m; i++ {
        score := scores[i] - delta
        if score > maxScore {
            maxScore = score
            middleEdge = MiddleEdge{
                FromRow: i,
                FromCol: midCol,
                ToRow:   i + 1,
                ToCol:   midCol,
                Score:   score,
            }
        }
    }
    
    // Check horizontal edges (from column j to j+1)
    if midCol < n {
        for i := 0; i <= m; i++ {
            score := scores[i] - delta
            if score > maxScore {
                maxScore = score
                middleEdge = MiddleEdge{
                    FromRow: i,
                    FromCol: midCol,
                    ToRow:   i,
                    ToCol:   midCol + 1,
                    Score:   score,
                }
            }
        }
    }
    
    // Check diagonal edges (from (i,j) to (i+1,j+1))
    if midCol < n && m > 0 {
        for i := 0; i < m; i++ {
            score := scores[i] + sigma
            if s[i] == t[midCol] {
                score += sigma
            } else {
                score -= sigma
            }
            if score > maxScore {
                maxScore = score
                middleEdge = MiddleEdge{
                    FromRow: i,
                    FromCol: midCol,
                    ToRow:   i + 1,
                    ToCol:   midCol + 1,
                    Score:   score,
                }
            }
        }
    }
    
    return middleEdge
}

// Alternative implementation with better space complexity
func findMiddleEdgeOptimized(s, t string, sigma int, delta int) MiddleEdge {
    m := len(s)
    n := len(t)
    
    midCol := n / 2
    
    // Forward pass - compute scores from left to middle
    forwardScores := make([]int, m+1)
    for i := 0; i <= m; i++ {
        forwardScores[i] = i * delta
    }
    
    for j := 1; j <= midCol; j++ {
        prevScores := make([]int, m+1)
        copy(prevScores, forwardScores)
        forwardScores[0] = j * delta
        
        for i := 1; i <= m; i++ {
            match := prevScores[i-1]
            if s[i-1] == t[j-1] {
                match += sigma
            } else {
                match -= sigma
            }
            
            delete := prevScores[i] - delta
            insert := forwardScores[i-1] - delta
            
            forwardScores[i] = max(match, delete, insert)
        }
    }
    
    // Backward pass - compute scores from right to middle
    backwardScores := make([]int, m+1)
    for i := 0; i <= m; i++ {
        backwardScores[i] = (m - i) * delta
    }
    
    for j := n - 1; j >= midCol; j-- {
        prevScores := make([]int, m+1)
        copy(prevScores, backwardScores)
        backwardScores[m] = (n - j) * delta
        
        for i := m - 1; i >= 0; i-- {
            match := prevScores[i+1]
            if s[i] == t[j] {
                match += sigma
            } else {
                match -= sigma
            }
            
            delete := prevScores[i] - delta
            insert := backwardScores[i+1] - delta
            
            backwardScores[i] = max(match, delete, insert)
        }
    }
    
    // Find the middle edge that maximizes score
    maxScore := math.MinInt32
    var middleEdge MiddleEdge
    
    // Check vertical edges (i -> i+1)
    for i := 0; i < m; i++ {
        score := forwardScores[i] + backwardScores[i+1] - delta
        if score > maxScore {
            maxScore = score
            middleEdge = MiddleEdge{
                FromRow: i,
                FromCol: midCol,
                ToRow:   i + 1,
                ToCol:   midCol,
                Score:   score,
            }
        }
    }
    
    // Check horizontal edges (j -> j+1)
    if midCol < n {
        for i := 0; i <= m; i++ {
            score := forwardScores[i] + backwardScores[i] - delta
            if score > maxScore {
                maxScore = score
                middleEdge = MiddleEdge{
                    FromRow: i,
                    FromCol: midCol,
                    ToRow:   i,
                    ToCol:   midCol + 1,
                    Score:   score,
                }
            }
        }
    }
    
    // Check diagonal edges (i,j -> i+1,j+1)
    if midCol < n && m > 0 {
        for i := 0; i < m; i++ {
            score := forwardScores[i] + backwardScores[i+1]
            if s[i] == t[midCol] {
                score += sigma
            } else {
                score -= sigma
            }
            if score > maxScore {
                maxScore = score
                middleEdge = MiddleEdge{
                    FromRow: i,
                    FromCol: midCol,
                    ToRow:   i + 1,
                    ToCol:   midCol + 1,
                    Score:   score,
                }
            }
        }
    }
    
    return middleEdge
}

// Helper function to find maximum of three integers
func max(a, b, c int) int {
    maxAB := a
    if b > a {
        maxAB = b
    }
    if c > maxAB {
        maxAB = c
    }
    return maxAB
}

// Helper function to print middle edge information
func printMiddleEdge(edge MiddleEdge) {
    fmt.Printf("Middle Edge:\n")
    fmt.Printf("  From: (%d, %d)\n", edge.FromRow, edge.FromCol)
    fmt.Printf("  To: (%d, %d)\n", edge.ToRow, edge.ToCol)
    fmt.Printf("  Score: %d\n", edge.Score)
}

func main() {
    // Example usage
    s := "ACGTACGT"
    t := "ACGTACGT"
    sigma := 1  // Match score
    delta := 1  // Mismatch score
    
    fmt.Println("Finding middle edge in alignment graph...")
    fmt.Printf("Sequence s: %s\n", s)
    fmt.Printf("Sequence t: %s\n", t)
    fmt.Printf("Sigma (match): %d\n", sigma)
    fmt.Printf("Delta (mismatch): %d\n", delta)
    fmt.Println()
    
    // Find middle edge using optimized approach
    edge := findMiddleEdgeOptimized(s, t, sigma, delta)
    printMiddleEdge(edge)
    
    // Test with different sequences
    fmt.Println("\n--- Test with different sequences ---")
    s2 := "ACGT"
    t2 := "ACGT"
    edge2 := findMiddleEdgeOptimized(s2, t2, sigma, delta)
    printMiddleEdge(edge2)
}
```

## Key Features

1. **Linear Space Complexity**: The algorithm uses O(m) space instead of O(m×n)
2. **Middle Edge Identification**: Finds the optimal edge that splits the alignment graph
3. **Dynamic Programming**: Implements proper scoring for alignment
4. **Edge Types**: Handles vertical, horizontal, and diagonal edges

## Algorithm Explanation

1. **Forward Pass**: Computes scores from left to middle column
2. **Backward Pass**: Computes scores from right to middle column  
3. **Middle Edge Calculation**: Combines forward and backward scores to find optimal edge
4. **Space Optimization**: Only stores necessary scores for middle column

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m) - linear space as required

This implementation efficiently finds the middle edge needed for the divide-and-conquer approach to sequence alignment, making it suitable for large sequences where memory usage is a concern.

