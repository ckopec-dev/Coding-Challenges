# Euler Problem - Hamming Distance Between Two Strings

The Hamming distance between two strings of equal length is the number of positions at which the corresponding characters are different.

## Problem Statement
Given two strings of equal length, compute their Hamming distance.

## Solution in Go

```go
package main

import (
    "fmt"
    "errors"
)

// hammingDistance computes the Hamming distance between two strings
func hammingDistance(s1, s2 string) (int, error) {
    // Check if strings have equal length
    if len(s1) != len(s2) {
        return 0, errors.New("strings must have equal length")
    }
    
    distance := 0
    for i := 0; i < len(s1); i++ {
        if s1[i] != s2[i] {
            distance++
        }
    }
    
    return distance, nil
}

func main() {
    // Test cases
    testCases := []struct {
        s1, s2   string
        expected int
    }{
        {"karolin", "kathrin", 3},
        {"karolin", "karolin", 0},
        {"1011101", "1001001", 2},
        {"2173896", "2233796", 3},
    }
    
    fmt.Println("Hamming Distance Tests:")
    fmt.Println("======================")
    
    for _, tc := range testCases {
        distance, err := hammingDistance(tc.s1, tc.s2)
        if err != nil {
            fmt.Printf("Error: %v\n", err)
            continue
        }
        
        status := "✓"
        if distance != tc.expected {
            status = "✗"
        }
        
        fmt.Printf("%s Strings: \"%s\" and \"%s\" -> Distance: %d (expected: %d)\n", 
                   status, tc.s1, tc.s2, distance, tc.expected)
    }
    
    // Test error case
    fmt.Println("\nError Case Test:")
    _, err := hammingDistance("short", "longer")
    if err != nil {
        fmt.Printf("✓ Correctly handled unequal length strings: %v\n", err)
    }
}
```

## Alternative Implementation Using Runes

For Unicode support, here's an implementation that handles Unicode characters:

```go
package main

import (
    "fmt"
    "errors"
)

// hammingDistanceUnicode computes Hamming distance with Unicode support
func hammingDistanceUnicode(s1, s2 string) (int, error) {
    // Convert to rune slices for proper Unicode handling
    r1 := []rune(s1)
    r2 := []rune(s2)
    
    if len(r1) != len(r2) {
        return 0, errors.New("strings must have equal length")
    }
    
    distance := 0
    for i := 0; i < len(r1); i++ {
        if r1[i] != r2[i] {
            distance++
        }
    }
    
    return distance, nil
}

func main() {
    // Unicode test case
    distance, err := hammingDistanceUnicode("Hello", "World")
    if err == nil {
        fmt.Printf("Unicode Hamming distance: %d\n", distance)
    }
}
```

## Output
```
Hamming Distance Tests:
======================
✓ Strings: "karolin" and "kathrin" -> Distance: 3 (expected: 3)
✓ Strings: "karolin" and "karolin" -> Distance: 0 (expected: 0)
✓ Strings: "1011101" and "1001001" -> Distance: 2 (expected: 2)
✓ Strings: "2173896" and "2233796" -> Distance: 3 (expected: 3)

Error Case Test:
✓ Correctly handled unequal length strings: strings must have equal length
```

## Key Features

1. **Input Validation**: Checks that both strings have equal length
2. **Error Handling**: Returns appropriate errors for invalid inputs
3. **Efficient Algorithm**: Single pass through the strings with O(n) time complexity
4. **Test Cases**: Comprehensive testing including edge cases
5. **Unicode Support**: Alternative implementation handles Unicode characters properly

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the strings
- **Space Complexity**: O(1) for the basic implementation, O(n) for the Unicode version due to rune conversion

This solution efficiently computes the Hamming distance between two strings while handling edge cases appropriately.

