# Euler Problem: Independent Segregation of Chromosomes

## Problem Statement
In the context of genetics, independent segregation of chromosomes refers to the principle that chromosomes are distributed independently during meiosis. This problem asks us to calculate the probability of obtaining a specific combination of chromosomes in gametes.

## Solution Approach
We'll use combinatorics to solve this problem. For n pairs of chromosomes, each chromosome can segregate independently into either of two gametes with equal probability (1/2).

## Ruby Implementation

```ruby
def independent_segregation_probability(n, k)
  # Calculate probability of getting exactly k chromosomes from one gamete
  # out of n total chromosome pairs
  #
  # This is a binomial probability: C(n,k) * (1/2)^n
  
  # Calculate combination C(n,k)
  combination = factorial(n) / (factorial(k) * factorial(n - k))
  
  # Calculate probability
  probability = combination * (1.0 / (2 ** n))
  
  probability
end

def factorial(n)
  return 1 if n <= 1
  (1..n).reduce(:*)
end

def calculate_all_probabilities(n)
  # Calculate probabilities for all possible numbers of chromosomes
  probabilities = {}
  
  (0..n).each do |k|
    probabilities[k] = independent_segregation_probability(n, k)
  end
  
  probabilities
end

# Example usage
puts "Independent Segregation of Chromosomes"
puts "======================================"

# For 3 chromosome pairs
n = 3
puts "\nWith #{n} chromosome pairs:"
probabilities = calculate_all_probabilities(n)

probabilities.each do |k, prob|
  puts "  #{k} chromosomes from one gamete: #{prob.round(6)}"
end

# For 5 chromosome pairs
n = 5
puts "\nWith #{n} chromosome pairs:"
probabilities = calculate_all_probabilities(n)

probabilities.each do |k, prob|
  puts "  #{k} chromosomes from one gamete: #{prob.round(6)}"
end

# Verification: sum of all probabilities should equal 1
total_prob = probabilities.values.sum
puts "\nSum of all probabilities: #{total_prob.round(10)}"

# Specific example: probability of getting exactly 2 chromosomes from one gamete
# with 4 chromosome pairs
n = 4
k = 2
result = independent_segregation_probability(n, k)
puts "\nProbability of getting exactly #{k} chromosomes from one gamete with #{n} pairs:"
puts "  #{result.round(6)}"

# Verification using binomial coefficient directly
puts "\nVerification using binomial coefficient:"
puts "C(#{n}, #{k}) = #{factorial(n) / (factorial(k) * factorial(n - k))}"
puts "(1/2)^#{n} = #{(1.0 / (2 ** n))}"
puts "Combined probability = #{(factorial(n) / (factorial(k) * factorial(n - k))) * (1.0 / (2 ** n))}"
```

## Expected Output

```
Independent Segregation of Chromosomes
======================================

With 3 chromosome pairs:
  0 chromosomes from one gamete: 0.125000
  1 chromosomes from one gamete: 0.375000
  2 chromosomes from one gamete: 0.375000
  3 chromosomes from one gamete: 0.125000

With 5 chromosome pairs:
  0 chromosomes from one gamete: 0.031250
  1 chromosomes from one gamete: 0.156250
  2 chromosomes from one gamete: 0.312500
  3 chromosomes from one gamete: 0.312500
  4 chromosomes from one gamete: 0.156250
  5 chromosomes from one gamete: 0.031250

Sum of all probabilities: 1.0000000000

Probability of getting exactly 2 chromosomes from one gamete with 4 pairs:
  0.375000

Verification using binomial coefficient:
C(4, 2) = 6
(1/2)^4 = 0.0625
Combined probability = 0.375
```

## Key Concepts

1. **Binomial Distribution**: The problem follows a binomial distribution where each chromosome pair independently segregates into one of two gametes.

2. **Combinatorial Mathematics**: The number of ways to choose k chromosomes from n pairs is given by the binomial coefficient C(n,k).

3. **Probability Calculation**: The probability is calculated as:
   ```
   P(X = k) = C(n,k) × (1/2)^n
   ```

4. **Independence**: Each chromosome pair segregates independently, making this a classic example of independent events in probability theory.

## Mathematical Formula

The probability of obtaining exactly k chromosomes from one gamete out of n chromosome pairs is:

$$P(X = k) = \binom{n}{k} \times \left(\frac{1}{2}\right)^n$$

Where:
- $\binom{n}{k}$ is the binomial coefficient (n choose k)
- $\left(\frac{1}{2}\right)^n$ is the probability of any specific segregation pattern

This solution correctly implements the mathematical principles behind independent segregation of chromosomes in genetics.

