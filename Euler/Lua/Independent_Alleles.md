# Euler Problem 007: Independent Alleles

## Problem Statement
A person has two alleles for each gene - one from each parent. In a population, alleles are distributed randomly. Given a population where each person has two alleles, and the probability of each allele being passed to offspring is 0.5, what is the probability that at least k individuals in the nth generation will have the dominant allele?

## Solution Approach
This is a binomial probability problem. We need to calculate the probability of having at least k individuals with the dominant allele in the nth generation.

In each generation, we're essentially performing a binomial experiment where:
- Each person has 2 alleles (2^n total alleles in generation n)
- Probability of dominant allele is 0.5
- We want P(X ≥ k) where X is the number of dominant alleles

## Lua Implementation

```lua
function factorial(n)
    if n <= 1 then return 1 end
    return n * factorial(n - 1)
end

function combination(n, k)
    if k > n or k < 0 then return 0 end
    return factorial(n) / (factorial(k) * factorial(n - k))
end

function binomial_probability(n, k, p)
    -- P(X = k) = C(n,k) * p^k * (1-p)^(n-k)
    return combination(n, k) * math.pow(p, k) * math.pow(1 - p, n - k)
end

function probability_at_least_k(n, k)
    -- Calculate P(X >= k) = 1 - P(X < k) = 1 - P(X <= k-1)
    local probability = 0
    for i = 0, k - 1 do
        probability = probability + binomial_probability(2^n, i, 0.5)
    end
    return 1 - probability
end

-- Main solution
function solve()
    local n = 7  -- 7th generation
    local k = 35 -- At least 35 individuals
    
    local result = probability_at_least_k(n, k)
    return string.format("%.4f", result)
end

-- Alternative approach using direct calculation
function solve_direct()
    local n = 7
    local k = 35
    
    -- In generation n, we have 2^n individuals
    local total_individuals = 2^n
    
    -- We want P(X >= k) where X ~ Binomial(total_individuals, 0.5)
    local probability = 0
    
    -- Calculate P(X >= k) = sum from i=k to n of C(n,i) * (0.5)^n
    for i = k, total_individuals do
        probability = probability + combination(total_individuals, i) * math.pow(0.5, total_individuals)
    end
    
    return string.format("%.4f", probability)
end

-- More efficient approach using cumulative distribution
function solve_efficient()
    local n = 7
    local k = 35
    
    local total_individuals = 2^n
    
    -- For large numbers, we can use the complement
    -- P(X >= k) = 1 - P(X < k) = 1 - P(X <= k-1)
    local cumulative = 0
    
    for i = 0, k - 1 do
        cumulative = cumulative + combination(total_individuals, i) * math.pow(0.5, total_individuals)
    end
    
    return string.format("%.4f", 1 - cumulative)
end

-- Execute the solution
print("Euler Problem 007 Solution:")
print("Probability of at least 35 individuals with dominant allele in 7th generation:")
print(solve_efficient())
```

## Explanation

The problem involves calculating binomial probabilities. In each generation:
1. We have 2^n individuals
2. Each individual has a 0.5 probability of having the dominant allele
3. We want the probability of having at least k individuals with the dominant allele

The solution uses:
- **Factorial function** to calculate combinations
- **Combination function** to compute C(n,k) = n!/(k!(n-k)!)
- **Binomial probability** to calculate individual probabilities
- **Cumulative probability** to find P(X ≥ k) using the complement

## Key Insights
- This is a classic binomial distribution problem
- The number of individuals in generation n is 2^n
- We need to calculate the cumulative probability of having fewer than k individuals with dominant alleles and subtract from 1
- For large values, direct calculation can be numerically unstable, so we use the complement approach

The final answer represents the probability that at least 35 individuals in the 7th generation will have the dominant allele.

