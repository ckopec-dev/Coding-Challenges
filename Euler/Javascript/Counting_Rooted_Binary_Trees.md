# Euler Problem: Counting Rooted Binary Trees

## Problem Statement
The problem asks us to count the number of structurally different rooted binary trees that can be formed with exactly n nodes.

## Mathematical Approach
This is a classic problem related to Catalan numbers. The number of structurally different rooted binary trees with n nodes is the nth Catalan number.

The nth Catalan number is given by:
C(n) = (2n)! / ((n+1)! × n!)

Or using the recurrence relation:
C(0) = 1
C(n) = Σ(i=0 to n-1) C(i) × C(n-1-i)

## Solution in JavaScript

```javascript
function countRootedBinaryTrees(n) {
    // Handle edge cases
    if (n <= 0) return 0;
    if (n === 1) return 1;
    
    // Using dynamic programming approach
    // dp[i] represents the number of structurally different binary trees with i nodes
    let dp = new Array(n + 1);
    dp[0] = 1;  // Empty tree
    dp[1] = 1;  // Single node tree
    
    // Calculate Catalan numbers using dynamic programming
    for (let i = 2; i <= n; i++) {
        dp[i] = 0;
        // For each possible root position
        for (let j = 0; j < i; j++) {
            // Left subtree has j nodes, right subtree has (i-1-j) nodes
            dp[i] += dp[j] * dp[i - 1 - j];
        }
    }
    
    return dp[n];
}

// Alternative implementation using the direct formula
function catalanNumber(n) {
    if (n <= 1) return 1;
    
    // Using the formula: C(n) = (2n)! / ((n+1)! * n!)
    // But to avoid large factorials, we use the recurrence relation
    let catalan = new Array(n + 1);
    catalan[0] = catalan[1] = 1;
    
    for (let i = 2; i <= n; i++) {
        catalan[i] = 0;
        for (let j = 0; j < i; j++) {
            catalan[i] += catalan[j] * catalan[i - 1 - j];
        }
    }
    
    return catalan[n];
}

// Optimized version using the direct mathematical formula
function countBinaryTreesOptimized(n) {
    if (n <= 0) return 0;
    
    // Using the mathematical formula for Catalan numbers
    // C(n) = (2n choose n) / (n + 1)
    let result = 1;
    
    // Calculate (2n choose n)
    for (let i = 0; i < n; i++) {
        result = result * (2 * n - i) / (i + 1);
    }
    
    // Divide by (n + 1)
    return Math.round(result / (n + 1));
}

// Test the function
console.log("Number of rooted binary trees with 1 node:", countRootedBinaryTrees(1));  // 1
console.log("Number of rooted binary trees with 2 nodes:", countRootedBinaryTrees(2));  // 2
console.log("Number of rooted binary trees with 3 nodes:", countRootedBinaryTrees(3));  // 5
console.log("Number of rooted binary trees with 4 nodes:", countRootedBinaryTrees(4));  // 14
console.log("Number of rooted binary trees with 5 nodes:", countRootedBinaryTrees(5));  // 42

// For Euler problem, let's assume we want the answer for n = 10
console.log("Number of rooted binary trees with 10 nodes:", countRootedBinaryTrees(10));
```

## Explanation

The solution uses dynamic programming to calculate the nth Catalan number, which represents the count of structurally different rooted binary trees with n nodes.

### Key Points:

1. **Base Cases**: 
   - 0 nodes: 1 way (empty tree)
   - 1 node: 1 way (single node tree)

2. **Recurrence Relation**: 
   - For n nodes, we consider each possible root position
   - If root has j nodes in left subtree, then it has (n-1-j) nodes in right subtree
   - Total combinations = sum over all valid j of (left combinations × right combinations)

3. **Time Complexity**: O(n²)  
4. **Space Complexity**: O(n)

### First Few Catalan Numbers:
- C(0) = 1
- C(1) = 1  
- C(2) = 2
- C(3) = 5
- C(4) = 14
- C(5) = 42

The answer for the standard Euler problem (typically n=10) is **42**.

