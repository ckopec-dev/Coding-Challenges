# Euler Problem: Find the Longest Substring Shared by Two Strings

## Problem Description
Given two strings, find the longest substring that appears in both strings.

## Solution in Go

```go
package main

import (
    "fmt"
    "strings"
)

// longestCommonSubstring finds the longest substring shared by two strings
func longestCommonSubstring(s1, s2 string) string {
    if s1 == "" || s2 == "" {
        return ""
    }
    
    // Create a 2D table to store lengths of common substrings
    m, n := len(s1), len(s2)
    dp := make([][]int, m+1)
    for i := range dp {
        dp[i] = make([]int, n+1)
    }
    
    // Variables to track the maximum length and ending position
    maxLength := 0
    endingPos := 0
    
    // Fill the dp table
    for i := 1; i <= m; i++ {
        for j := 1; j <= n; j++ {
            if s1[i-1] == s2[j-1] {
                dp[i][j] = dp[i-1][j-1] + 1
                if dp[i][j] > maxLength {
                    maxLength = dp[i][j]
                    endingPos = i
                }
            } else {
                dp[i][j] = 0
            }
        }
    }
    
    // Extract the longest common substring
    if maxLength == 0 {
        return ""
    }
    
    start := endingPos - maxLength
    return s1[start:endingPos]
}

// Alternative approach using brute force (less efficient but simpler)
func longestCommonSubstringBruteForce(s1, s2 string) string {
    if s1 == "" || s2 == "" {
        return ""
    }
    
    longest := ""
    
    // Check all substrings of s1
    for i := 0; i < len(s1); i++ {
        for j := i + 1; j <= len(s1); j++ {
            substring := s1[i:j]
            if strings.Contains(s2, substring) && len(substring) > len(longest) {
                longest = substring
            }
        }
    }
    
    return longest
}

// Optimized version that returns all longest common substrings
func allLongestCommonSubstrings(s1, s2 string) []string {
    if s1 == "" || s2 == "" {
        return []string{}
    }
    
    m, n := len(s1), len(s2)
    dp := make([][]int, m+1)
    for i := range dp {
        dp[i] = make([]int, n+1)
    }
    
    maxLength := 0
    result := []string{}
    
    for i := 1; i <= m; i++ {
        for j := 1; j <= n; j++ {
            if s1[i-1] == s2[j-1] {
                dp[i][j] = dp[i-1][j-1] + 1
                if dp[i][j] > maxLength {
                    maxLength = dp[i][j]
                    result = []string{s1[i-maxLength : i]}
                } else if dp[i][j] == maxLength && maxLength > 0 {
                    substring := s1[i-maxLength : i]
                    // Avoid duplicates
                    found := false
                    for _, existing := range result {
                        if existing == substring {
                            found = true
                            break
                        }
                    }
                    if !found {
                        result = append(result, substring)
                    }
                }
            } else {
                dp[i][j] = 0
            }
        }
    }
    
    return result
}

func main() {
    // Test cases
    testCases := []struct {
        s1, s2 string
    }{
        {"abcdxyz", "xyzabcd"},
        {"zxabcdezy", "yzabcdezx"},
        {"GeeksforGeeks", "GeeksQuiz"},
        {"", "abc"},
        {"abc", ""},
        {"same", "same"},
        {"programming", "algorithm"},
    }
    
    fmt.Println("Longest Common Substring Solutions:")
    fmt.Println("===================================")
    
    for i, tc := range testCases {
        result1 := longestCommonSubstring(tc.s1, tc.s2)
        result2 := longestCommonSubstringBruteForce(tc.s1, tc.s2)
        allResults := allLongestCommonSubstrings(tc.s1, tc.s2)
        
        fmt.Printf("Test Case %d:\n", i+1)
        fmt.Printf("  String 1: \"%s\"\n", tc.s1)
        fmt.Printf("  String 2: \"%s\"\n", tc.s2)
        fmt.Printf("  Longest Common Substring (DP): \"%s\"\n", result1)
        fmt.Printf("  Longest Common Substring (Brute Force): \"%s\"\n", result2)
        fmt.Printf("  All Longest Common Substrings: %v\n", allResults)
        fmt.Println()
    }
    
    // Example with the classic problem
    fmt.Println("Classic Example:")
    fmt.Println("================")
    s1 := "GeeksforGeeks"
    s2 := "GeeksQuiz"
    result := longestCommonSubstring(s1, s2)
    fmt.Printf("String 1: \"%s\"\n", s1)
    fmt.Printf("String 2: \"%s\"\n", s2)
    fmt.Printf("Longest Common Substring: \"%s\"\n", result)
    fmt.Printf("Length: %d\n", len(result))
}
```

## Algorithm Explanation

### Dynamic Programming Approach (Recommended)
1. **Create a 2D table** `dp[i][j]` where each cell represents the length of common substring ending at position `i-1` in `s1` and position `j-1` in `s2`
2. **Fill the table**:
   - If `s1[i-1] == s2[j-1]`, then `dp[i][j] = dp[i-1][j-1] + 1`
   - Otherwise, `dp[i][j] = 0`
3. **Track maximum** length and ending position to extract the actual substring

### Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are lengths of the strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Output
```
Longest Common Substring Solutions:
===================================
Test Case 1:
  String 1: "abcdxyz"
  String 2: "xyzabcd"
  Longest Common Substring (DP): "abcd"
  Longest Common Substring (Brute Force): "abcd"
  All Longest Common Substrings: [abcd]

Test Case 2:
  String 1: "zxabcdezy"
  String 2: "yzabcdezx"
  Longest Common Substring (DP): "abcdez"
  Longest Common Substring (Brute Force): "abcdez"
  All Longest Common Substrings: [abcdez]

Classic Example:
================
String 1: "GeeksforGeeks"
String 2: "GeeksQuiz"
Longest Common Substring: "Geeks"
Length: 5
```

## Key Features
1. **Efficient DP solution** with optimal time complexity
2. **Brute force alternative** for comparison and understanding
3. **Handles edge cases** like empty strings
4. **Returns all longest substrings** when there are multiple with same maximum length
5. **Well-documented** with clear explanations and test cases

The dynamic programming approach is the most efficient solution for this problem, providing optimal performance for large inputs while maintaining clear logic and readability.

