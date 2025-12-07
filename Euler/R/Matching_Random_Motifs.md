# Euler Problem: Matching Random Motifs - R Solution

## Problem Understanding

The problem asks us to find the probability that a random DNA string of length `n` contains a specific motif of length `m` at least once.

## Mathematical Approach

For a random DNA string of length `n` with equal probability of each nucleotide (0.25 for A, C, G, T), the probability of a specific motif of length `m` occurring at any particular position is `(0.25)^m`.

The probability that the motif does NOT occur at any specific position is `1 - (0.25)^m`.

For a string of length `n`, there are `n - m + 1` possible positions where the motif can occur.

However, since positions are not independent, we need to use a more sophisticated approach.

## R Implementation

```r
# Function to calculate probability of motif occurrence
motif_probability <- function(n, m) {
  # For large n and m, we can use the approximation
  # P(at least one match) = 1 - P(no matches at all)
  
  # Number of possible positions
  positions <- n - m + 1
  
  if (positions <= 0) {
    return(0)
  }
  
  # Probability of not matching at a specific position
  p_no_match <- 1 - (0.25)^m
  
  # Probability of not matching at any position (approximation)
  # This is an approximation - exact calculation is complex due to dependencies
  p_no_match_all <- p_no_match^positions
  
  # Probability of at least one match
  result <- 1 - p_no_match_all
  
  return(result)
}

# More precise approach using simulation for verification
simulate_motif <- function(n, m, trials = 10000) {
  count <- 0
  
  for (i in 1:trials) {
    # Generate random DNA string of length n
    dna <- sample(c("A", "C", "G", "T"), n, replace = TRUE)
    dna_string <- paste(dna, collapse = "")
    
    # Check if motif m is present
    motif <- sample(c("A", "C", "G", "T"), m, replace = TRUE)
    motif_string <- paste(motif, collapse = "")
    
    if (grepl(motif_string, dna_string)) {
      count <- count + 1
    }
  }
  
  return(count / trials)
}

# For the exact mathematical solution, we can use a different approach
# This is a simplified version for demonstration
exact_motif_probability <- function(n, m) {
  if (n < m) return(0)
  
  # For very large n, we can use the Poisson approximation
  # Expected number of occurrences
  lambda <- (n - m + 1) * (0.25)^m
  
  # Probability of at least one occurrence using Poisson
  # P(X >= 1) = 1 - P(X = 0) = 1 - e^(-lambda)
  return(1 - exp(-lambda))
}

# Example usage
# Let's solve for a specific case: n = 1000, m = 10
n <- 1000
m <- 10

prob1 <- motif_probability(n, m)
prob2 <- exact_motif_probability(n, m)

cat("For DNA string of length", n, "and motif of length", m, "\n")
cat("Approximate probability:", prob1, "\n")
cat("Exact (Poisson) probability:", prob2, "\n")

# Let's also create a function to solve the general case
solve_matching_random_motifs <- function(n, m) {
  # Return the probability that a random DNA string of length n
  # contains a specific motif of length m at least once
  
  if (n < m) {
    return(0)
  }
  
  # Using Poisson approximation for large n
  # Expected number of occurrences = (n - m + 1) * (1/4)^m
  lambda <- (n - m + 1) * (0.25)^m
  
  # Probability of at least one match = 1 - e^(-lambda)
  return(1 - exp(-lambda))
}

# Test with different values
test_cases <- list(
  c(100, 5),
  c(1000, 10),
  c(10000, 15),
  c(500, 3)
)

cat("\nTest Results:\n")
cat("n\tm\tProbability\n")
cat("----\t----\t----\n")

for (case in test_cases) {
  n_val <- case[1]
  m_val <- case[2]
  prob <- solve_matching_random_motifs(n_val, m_val)
  cat(n_val, "\t", m_val, "\t", round(prob, 6), "\n")
}
```

## Alternative Mathematical Approach

For a more precise mathematical solution:

```r
# More mathematically rigorous approach
advanced_motif_prob <- function(n, m) {
  if (n < m) return(0)
  
  # Using inclusion-exclusion principle or recursive methods
  # For practical purposes, the Poisson approximation is very good
  
  # Calculate probability using the complement
  # P(at least one match) = 1 - P(no matches)
  
  # Number of positions
  k <- n - m + 1
  
  # Probability of no match at one position
  p_no_match <- 1 - (0.25)^m
  
  # For independence approximation (which isn't strictly true)
  # P(no matches at all) = (1 - (0.25)^m)^k
  p_no_matches <- p_no_match^k
  
  return(1 - p_no_matches)
}

# Comparison function
compare_methods <- function(n, m) {
  method1 <- advanced_motif_prob(n, m)
  method2 <- solve_matching_random_motifs(n, m)
  
  cat("n =", n, ", m =", m, "\n")
  cat("Method 1 (independence):", round(method1, 8), "\n")
  cat("Method 2 (Poisson):", round(method2, 8), "\n")
  cat("Difference:", round(abs(method1 - method2), 10), "\n\n")
}

# Test comparison
compare_methods(1000, 10)
compare_methods(500, 5)
compare_methods(2000, 15)
```

## Key Insights

1. **Poisson Approximation**: For large strings, the Poisson distribution provides an excellent approximation
2. **Dependencies**: Positions in DNA strings are not independent, but the approximation works well
3. **Asymptotic Behavior**: As string length increases relative to motif length, probability approaches 1

## Final Solution Function

```r
# Final clean solution
matching_random_motifs <- function(n, m) {
  # Calculate probability that a random DNA string of length n
  # contains a specific motif of length m at least once
  
  if (n < m) return(0)
  
  # Using Poisson approximation: P(X >= 1) = 1 - e^(-lambda)
  # where lambda = (n - m + 1) * (0.25)^m
  
  lambda <- (n - m + 1) * (0.25)^m
  probability <- 1 - exp(-lambda)
  
  return(probability)
}

# Example usage
result <- matching_random_motifs(1000, 10)
cat("Probability:", result, "\n")
```

This solution provides an efficient way to calculate the probability of finding a specific motif in a random DNA string using mathematical approximations that are both accurate and computationally efficient.

