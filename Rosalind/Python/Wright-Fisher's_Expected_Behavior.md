# Rosalind Problem: Wright-Fisher's Expected Behavior

## Problem Description
In the Wright-Fisher model of genetic drift, we consider a population of N diploid individuals, where each individual has two alleles at each locus. The model assumes random mating and no selection, mutation, or migration. We want to calculate the expected number of copies of a particular allele after one generation.

## Solution Approach
The Wright-Fisher model describes how allele frequencies change over time due to genetic drift. For a population of N diploid individuals (2N total alleles), if we start with k copies of an allele, the expected number of copies after one generation follows a binomial distribution.

## Python Implementation

```python
def wright_fisher_expected_behavior(N, k):
    """
    Calculate the expected number of copies of an allele after one generation
    in the Wright-Fisher model.
    
    Parameters:
    N (int): Population size (number of diploid individuals)
    k (int): Number of copies of the allele in the current generation
    
    Returns:
    float: Expected number of copies after one generation
    """
    # In Wright-Fisher model, the expected number of copies
    # remains the same as the initial frequency
    # E[X_{t+1}] = E[X_t] = k/(2N)
    # But since we want the expected number of copies, not frequency:
    # E[copies] = 2N * (k/(2N)) = k
    # Wait, that's not right. Let me reconsider...
    
    # Actually, the expected number of copies of the allele in the next generation
    # is the same as the current number of copies due to the properties of 
    # the Wright-Fisher model (it's a martingale)
    return k

def wright_fisher_variance(N, k):
    """
    Calculate the variance of the number of copies of an allele after one generation.
    
    Parameters:
    N (int): Population size (number of diploid individuals)
    k (int): Number of copies of the allele in the current generation
    
    Returns:
    float: Variance of copies after one generation
    """
    # Variance in Wright-Fisher model
    # Var[X_{t+1}] = (k/(2N)) * (1 - k/(2N)) * 2N
    # This is the variance of a binomial distribution with n=2N and p=k/(2N)
    p = k / (2 * N)
    variance = 2 * N * p * (1 - p)
    return variance

def solve_wright_fisher(N, k):
    """
    Solve the Wright-Fisher expected behavior problem.
    
    Parameters:
    N (int): Population size (number of diploid individuals)
    k (int): Number of copies of the allele in the current generation
    
    Returns:
    tuple: (expected_copies, variance)
    """
    expected = wright_fisher_expected_behavior(N, k)
    variance = wright_fisher_variance(N, k)
    
    return expected, variance

# Example usage
if __name__ == "__main__":
    # Example from problem
    N = 4  # Population size (4 diploid individuals)
    k = 2  # Number of copies of allele
    
    expected, variance = solve_wright_fisher(N, k)
    
    print(f"Population size (N): {N}")
    print(f"Number of copies (k): {k}")
    print(f"Expected copies after one generation: {expected}")
    print(f"Variance of copies: {variance}")
    
    # Another example
    N2 = 10
    k2 = 5
    
    expected2, variance2 = solve_wright_fisher(N2, k2)
    
    print(f"\nPopulation size (N): {N2}")
    print(f"Number of copies (k): {k2}")
    print(f"Expected copies after one generation: {expected2}")
    print(f"Variance of copies: {variance2}")
```

## Alternative Approach (More Detailed)

```python
import math

def wright_fisher_expected_behavior_detailed(N, k):
    """
    Detailed calculation of Wright-Fisher expected behavior.
    This approach calculates the expected value using the properties of
    the binomial distribution.
    
    Parameters:
    N (int): Population size (number of diploid individuals)
    k (int): Number of copies of the allele in the current generation
    
    Returns:
    float: Expected number of copies after one generation
    """
    # In Wright-Fisher model, each individual in the next generation
    # is chosen independently from the current generation with replacement
    # The number of copies follows a binomial distribution
    # n = 2N (total alleles in next generation)
    # p = k/(2N) (probability of choosing an allele of interest)
    
    n = 2 * N
    p = k / n
    
    # Expected value of binomial distribution: E[X] = n * p
    expected = n * p
    
    return expected

def wright_fisher_solution(N, k):
    """
    Main solution function for Wright-Fisher problem.
    
    Parameters:
    N (int): Population size (number of diploid individuals)
    k (int): Number of copies of the allele in the current generation
    
    Returns:
    list: [expected_copies, variance]
    """
    # Expected number of copies
    expected = wright_fisher_expected_behavior_detailed(N, k)
    
    # Variance of the binomial distribution
    n = 2 * N
    p = k / n
    variance = n * p * (1 - p)
    
    return [expected, variance]

# Test with sample data
def main():
    # Sample input from Rosalind problem
    N = 4
    k = 2
    
    result = wright_fisher_solution(N, k)
    print(f"Expected copies: {result[0]}")
    print(f"Variance: {result[1]}")
    
    # Format for submission
    print(f"{result[0]:.6f} {result[1]:.6f}")

if __name__ == "__main__":
    main()
```

## Key Points

1. **Wright-Fisher Model**: Models genetic drift in populations with random mating and no selection
2. **Expected Value**: In the Wright-Fisher model, the expected number of copies of an allele remains the same as the initial number due to the martingale property
3. **Variance**: The variance decreases over time due to genetic drift
4. **Population Size**: Larger populations experience less drift (lower variance)

## Time Complexity
- **Time**: O(1) - constant time operations
- **Space**: O(1) - constant space usage

This solution handles the mathematical properties of the Wright-Fisher model correctly and provides the expected behavior for genetic drift in populations.

