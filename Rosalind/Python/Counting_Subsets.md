# Rosalind Problem: Counting Subsets

## Problem Statement
Given: A positive integer n ≤ 1000.

Return: The total number of subsets of {1,2,…,n} modulo 1,000,000.

## Solution

```python
def count_subsets(n):
    """
    Count the number of subsets of {1,2,...,n}
    
    For a set with n elements, the total number of subsets is 2^n
    We need to return this modulo 1,000,000
    
    Args:
        n (int): Positive integer representing the size of the set
    
    Returns:
        int: Number of subsets modulo 1,000,000
    """
    MOD = 1000000
    return pow(2, n, MOD)

# Read input and solve
if __name__ == "__main__":
    # Read the input
    n = int(input().strip())
    
    # Calculate and print the result
    result = count_subsets(n)
    print(result)
```

## Explanation

The key insight to this problem is understanding that for any set with n elements, the total number of subsets is 2^n.

This can be proven by considering that for each element in the set, we have two choices:
1. Include the element in the subset
2. Exclude the element from the subset

Since we make this choice independently for each of the n elements, the total number of combinations is 2 × 2 × ... × 2 (n times) = 2^n.

For example:
- n = 1: Set {1} has subsets: {}, {1} → 2^1 = 2 subsets
- n = 2: Set {1,2} has subsets: {}, {1}, {2}, {1,2} → 2^2 = 4 subsets
- n = 3: Set {1,2,3} has 2^3 = 8 subsets

Since the result can be very large, we take it modulo 1,000,000 as required.

## Example

**Input:**
```
3
```

**Output:**
```
8
```

**Explanation:**
For n = 3, we have 2^3 = 8 subsets, which is 8 modulo 1,000,000.

## Time and Space Complexity

- **Time Complexity:** O(log n) due to the efficient modular exponentiation using `pow(2, n, MOD)`
- **Space Complexity:** O(1) - only using constant extra space

The solution uses Python's built-in `pow` function with three arguments, which efficiently computes (base^exponent) % modulus using modular exponentiation, avoiding potential overflow issues with large numbers.

