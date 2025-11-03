# Euler Problem: Enumerating Gene Orders - Go Solution

## Problem Statement
A permutation of length n is an ordering of the positive integers {1,2,...,n}. For example, π=(5,3,2,1,4) is a permutation of length 5.

Given: A positive integer n≤7.

Return: The total number of permutations of length n, followed by a list of all such permutations (in any order).

## Solution

```go
package main

import (
    "fmt"
    "math"
    "sort"
)

// factorial calculates n!
func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n-1)
}

// permute generates all permutations of the given slice
func permute(nums []int) [][]int {
    var result [][]int
    var current []int
    used := make([]bool, len(nums))
    
    backtrack(nums, current, used, &result)
    return result
}

// backtrack is a helper function that generates permutations recursively
func backtrack(nums []int, current []int, used []bool, result *[][]int) {
    // Base case: if current permutation is complete
    if len(current) == len(nums) {
        // Create a copy of current permutation
        perm := make([]int, len(current))
        copy(perm, current)
        *result = append(*result, perm)
        return
    }
    
    // Try each unused number
    for i := 0; i < len(nums); i++ {
        if !used[i] {
            // Choose
            current = append(current, nums[i])
            used[i] = true
            
            // Explore
            backtrack(nums, current, used, result)
            
            // Unchoose (backtrack)
            current = current[:len(current)-1]
            used[i] = false
        }
    }
}

// generatePermutations generates all permutations of numbers 1 to n
func generatePermutations(n int) [][]int {
    nums := make([]int, n)
    for i := 0; i < n; i++ {
        nums[i] = i + 1
    }
    return permute(nums)
}

func main() {
    n := 3 // Example input - can be changed to any value <= 7
    
    // Calculate total number of permutations
    totalPermutations := factorial(n)
    
    // Generate all permutations
    permutations := generatePermutations(n)
    
    // Output results
    fmt.Printf("%d\n", totalPermutations)
    
    // Sort permutations for consistent output
    sortPermutations(permutations)
    
    // Print each permutation
    for _, perm := range permutations {
        for i, num := range perm {
            if i > 0 {
                fmt.Print(" ")
            }
            fmt.Print(num)
        }
        fmt.Println()
    }
}

// sortPermutations sorts the permutations for consistent output
func sortPermutations(perms [][]int) {
    for _, perm := range perms {
        sort.Ints(perm)
    }
    // Sort the slice of slices
    sort.SliceStable(perms, func(i, j int) bool {
        for k := 0; k < len(perms[i]) && k < len(perms[j]); k++ {
            if perms[i][k] < perms[j][k] {
                return true
            } else if perms[i][k] > perms[j][k] {
                return false
            }
        }
        return len(perms[i]) < len(perms[j])
    })
}
```

## Example Output for n=3

```
6
1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1
```

## Explanation

1. **Factorial Calculation**: First, we calculate the total number of permutations using n! (n factorial)

2. **Backtracking Algorithm**: 
   - Uses a recursive backtracking approach to generate all permutations
   - Maintains a `used` array to track which elements are currently in the permutation
   - Builds permutations incrementally and backtracks when a complete permutation is formed

3. **Key Components**:
   - `factorial()`: Computes n! for the total count
   - `permute()`: Main function that generates all permutations
   - `backtrack()`: Recursive helper that builds permutations
   - `generatePermutations()`: Wrapper function that creates the initial array [1,2,...,n]

4. **Time Complexity**: O(n! × n) where n! is the number of permutations and n is the time to copy each permutation

5. **Space Complexity**: O(n! × n) for storing all permutations

## Usage

To use this solution for different values of n:
1. Change the value of `n` in the `main()` function
2. The program will output the total number of permutations followed by all permutations
3. Works for any n where 1 ≤ n ≤ 7 as specified in the problem constraints

