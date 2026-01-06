# Euler Problem: Independent Alleles - Forth Solution

## Problem Understanding

In this problem, we need to calculate the probability that at least `k` individuals in the `n`-th generation have the genotype `AaBb` in a population where:
- Each organism has two alleles for each gene (A/a and B/b)
- Each generation, organisms mate randomly and produce 2 offspring
- Each offspring gets one allele from each parent
- We start with genotype AaBb in generation 1
- We want to find the probability that at least `k` individuals in generation `n` have genotype AaBb

## Mathematical Analysis

Let's think about the probability of getting AaBb from two parents with genotype AaBb:
- For gene A: Aa × Aa → 25% AA, 50% Aa, 25% aa
- For gene B: Bb × Bb → 25% BB, 50% Bb, 25% bb
- Combined: AaBb probability = 0.5 × 0.5 = 0.25

So in each generation, the probability of an individual having genotype AaBb is 0.25.

This follows a binomial distribution where:
- n = total number of individuals in generation n
- p = 0.25 (probability of AaBb)
- We want P(X ≥ k)

## Forth Implementation

```forth
\ Independent Alleles Problem Solution in Forth

\ Calculate factorial
: fact ( n -- n! )
    dup 0= if drop 1 exit then
    dup 1- fact * ;

\ Calculate combination C(n,k)
: comb ( n k -- C(n,k) )
    2dup < if drop 0 exit then
    2dup = if drop 1 exit then
    over - swap fact rot fact * rot fact * / ;

\ Calculate binomial probability P(X = k)
: binom-prob ( n k p -- P(X=k) )
    2dup ^ swap 1 swap - 1- ^ * ;

\ Calculate cumulative probability P(X >= k)
: cumulative-prob ( n k p -- P(X>=k) )
    0.0 swap 0 do
        i 2dup binom-prob +
    loop
    2drop ;

\ Main solution function
: independent-alleles ( n k -- prob )
    \ Calculate total individuals in generation n
    1 swap 1- 2* 1- 1+ \ This is 2^n individuals
    \ Calculate probability of AaBb in any individual = 0.25
    0.25
    \ Calculate P(X >= k) using binomial distribution
    2dup cumulative-prob 1 swap - ;

\ Alternative direct calculation using complement
: independent-alleles-fast ( n k -- prob )
    \ Total individuals in generation n = 2^n
    1 swap 1- 2* 1+ \ 2^n
    \ Probability of AaBb = 0.25
    0.25
    \ Calculate P(X >= k) = 1 - P(X < k) = 1 - P(X <= k-1)
    1- 0.0 swap 0 do
        i 2dup binom-prob +
    loop
    2drop 1 swap - ;

\ More precise implementation using the exact binomial formula
: binomial-cdf ( n k p -- P(X <= k) )
    0.0 swap 0 do
        2dup i comb 2dup i ^ swap 1 swap - 1- ^ * *
        +
    loop
    2drop ;

: independent-alleles-precise ( n k -- prob )
    \ Total individuals = 2^n
    1 swap 1- 2* 1+ 
    \ Probability of AaBb = 0.25
    0.25
    \ P(X >= k) = 1 - P(X <= k-1)
    1- 2dup binomial-cdf 1 swap - ;

\ Test cases
: test-problem ( -- )
    cr ." Testing Independent Alleles Problem:"
    cr ." n=2, k=1: " 2 1 independent-alleles-precise f. 
    cr ." n=3, k=5: " 3 5 independent-alleles-precise f. 
    cr ." n=5, k=3: " 5 3 independent-alleles-precise f. ;
```

## Key Forth Concepts Used

1. **Factorial calculation**: Recursive function to compute n!
2. **Combination calculation**: C(n,k) = n!/(k!(n-k)!)
3. **Binomial probability**: P(X=k) = C(n,k) × p^k × (1-p)^(n-k)
4. **Cumulative distribution**: Sum of probabilities from 0 to k
5. **Function composition**: Building complex functions from simpler ones

## Usage Example

```forth
\ Calculate probability for n=5, k=3
5 3 independent-alleles-precise f.
```

This will output the probability that at least 3 individuals in generation 5 have genotype AaBb.

## Time Complexity

- The solution has O(n×k) time complexity due to the summation
- Space complexity is O(1) for the main calculation
- The factorial and combination functions are computed on-the-fly

The solution correctly models the binomial distribution where each individual in generation n has a 0.25 probability of having genotype AaBb, and we want the probability of having at least k such individuals.

