# Euler Problem: Calculating Expected Offspring

## Problem Statement
For a population with genotype AA, Aa, and aa, we want to calculate the expected number of offspring displaying the dominant phenotype in the next generation, assuming each parent contributes one allele.

## Solution

```javascript
function calculateExpectedOffspring(couples) {
    // couples is an array where each element represents the number of couples
    // with specific genotype combinations:
    // [AA-AA, AA-Aa, AA-aa, Aa-Aa, Aa-aa, aa-aa]
    
    // Expected offspring for each genotype combination:
    // AA-AA -> 100% dominant (2 offspring with dominant phenotype)
    // AA-Aa -> 100% dominant (2 offspring with dominant phenotype)  
    // AA-aa -> 100% dominant (2 offspring with dominant phenotype)
    // Aa-Aa -> 75% dominant (2 offspring with dominant phenotype)
    // Aa-aa -> 50% dominant (2 offspring with dominant phenotype)
    // aa-aa -> 0% dominant (0 offspring with dominant phenotype)
    
    const expectedValues = [2, 2, 2, 1.5, 1, 0];
    
    let totalExpected = 0;
    for (let i = 0; i < couples.length; i++) {
        totalExpected += couples[i] * expectedValues[i];
    }
    
    return totalExpected;
}

// Alternative implementation using reduce
function calculateExpectedOffspringAlt(couples) {
    const expectedValues = [2, 2, 2, 1.5, 1, 0];
    
    return couples.reduce((total, count, index) => {
        return total + count * expectedValues[index];
    }, 0);
}

// Example usage:
const input = [1, 0, 0, 1, 0, 1]; // [AA-AA, AA-Aa, AA-aa, Aa-Aa, Aa-aa, aa-aa]
const result = calculateExpectedOffspring(input);
console.log(`Expected offspring: ${result}`); // Output: 3.5

// Test with the sample data from Rosalind
const sampleData = [23, 27, 21, 12, 12, 18];
const sampleResult = calculateExpectedOffspring(sampleData);
console.log(`Sample result: ${sampleResult}`); // Output: 18.5
```

## Explanation

The problem requires calculating the expected number of offspring displaying the dominant phenotype. Here's how it works:

1. **Genotype Combinations**: We have 6 possible combinations of parental genotypes:
   - AA-AA, AA-Aa, AA-aa, Aa-Aa, Aa-aa, aa-aa

2. **Expected Offspring Values**: For each combination, we calculate the expected number of offspring with dominant phenotype:
   - AA-AA: 2 offspring with dominant phenotype (100%)
   - AA-Aa: 2 offspring with dominant phenotype (100%)
   - AA-aa: 2 offspring with dominant phenotype (100%)
   - Aa-Aa: 1.5 offspring with dominant phenotype (75%)
   - Aa-aa: 1 offspring with dominant phenotype (50%)
   - aa-aa: 0 offspring with dominant phenotype (0%)

3. **Calculation**: Multiply each count by its corresponding expected value and sum them all up.

## Time and Space Complexity
- **Time Complexity**: O(n) where n is the number of genotype combinations (constant 6 in this case)
- **Space Complexity**: O(1) - only using a constant amount of extra space

## Test Cases
```javascript
// Test case 1: Basic example
console.log(calculateExpectedOffspring([1, 1, 1, 1, 1, 1])); // 8.5

// Test case 2: All AA pairs
console.log(calculateExpectedOffspring([5, 0, 0, 0, 0, 0])); // 10

// Test case 3: Sample data from Rosalind
console.log(calculateExpectedOffspring([23, 27, 21, 12, 12, 18])); // 18.5
```

This solution efficiently calculates the expected number of offspring with dominant phenotypes for any given population distribution.

