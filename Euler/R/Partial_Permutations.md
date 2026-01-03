# Euler Problem: Partial Permutations

Let me solve this step by step using R.

## Problem Understanding

The problem asks us to find the number of partial permutations of k objects chosen from n objects, where we only consider arrangements of k objects from n, not all n objects.

In combinatorics, this is the number of permutations of k objects chosen from n objects, which is:
P(n,k) = n! / (n-k)!

## Solution in R

```r
# Function to calculate partial permutations P(n,k)
partial_permutations <- function(n, k) {
  if (k > n) return(0)
  if (k == 0) return(1)
  
  # Calculate n! / (n-k)! = n * (n-1) * ... * (n-k+1)
  result <- 1
  for (i in (n-k+1):n) {
    result <- result * i
  }
  return(result)
}

# Alternative implementation using factorial
partial_permutations_alt <- function(n, k) {
  if (k > n) return(0)
  if (k == 0) return(1)
  
  # Using factorial formula: n! / (n-k)!
  return(factorial(n) / factorial(n-k))
}

# For large numbers, we might want to use modular arithmetic
partial_permutations_mod <- function(n, k, mod = 1000000) {
  if (k > n) return(0)
  if (k == 0) return(1)
  
  result <- 1
  for (i in (n-k+1):n) {
    result <- (result * i) %% mod
  }
  return(result)
}

# Example usage
cat("Partial permutations P(7, 4):", partial_permutations(7, 4), "\n")
cat("Partial permutations P(10, 3):", partial_permutations(10, 3), "\n")
cat("Partial permutations P(5, 5):", partial_permutations(5, 5), "\n")

# Verification with small examples
# P(7,4) = 7!/(7-4)! = 7!/3! = 7*6*5*4 = 840
cat("Verification P(7,4):", partial_permutations(7, 4), "\n")

# P(10,3) = 10!/(10-3)! = 10!/7! = 10*9*8 = 720
cat("Verification P(10,3):", partial_permutations(10, 3), "\n")

# For very large numbers, use modular arithmetic
cat("P(1000, 10) mod 1000000:", partial_permutations_mod(1000, 10), "\n")
```

## Complete Solution with Modular Arithmetic

```r
# More robust implementation for large numbers
partial_permutations <- function(n, k, mod = NULL) {
  # Handle edge cases
  if (k > n) return(ifelse(is.null(mod), 0, 0))
  if (k == 0) return(ifelse(is.null(mod), 1, 1))
  if (k == n) return(ifelse(is.null(mod), factorial(n), factorial(n) %% mod))
  
  # Calculate P(n,k) = n * (n-1) * ... * (n-k+1)
  result <- 1
  for (i in (n-k+1):n) {
    if (is.null(mod)) {
      result <- result * i
    } else {
      result <- (result * i) %% mod
    }
  }
  
  return(result)
}

# Test cases
cat("P(7,4) =", partial_permutations(7, 4), "\n")
cat("P(10,3) =", partial_permutations(10, 3), "\n")
cat("P(5,2) =", partial_permutations(5, 2), "\n")
cat("P(10,0) =", partial_permutations(10, 0), "\n")
cat("P(3,5) =", partial_permutations(3, 5), "\n")

# With modular arithmetic
cat("P(1000, 10) mod 1000000 =", partial_permutations(1000, 10, mod = 1000000), "\n")
```

## Key Points

1. **Formula**: P(n,k) = n!/(n-k)! = n × (n-1) × ... × (n-k+1)
2. **Edge cases**: 
   - If k > n, return 0
   - If k = 0, return 1
   - If k = n, return n!
3. **Efficiency**: Direct calculation is more efficient than computing factorials separately
4. **Modular arithmetic**: For very large numbers, use modular arithmetic to prevent overflow

The function handles all standard cases and provides both regular and modular arithmetic versions for different use cases.

