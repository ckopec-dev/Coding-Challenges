# Euler Problem: Linguistic Complexity of a Genome

## Problem Description

The linguistic complexity of a genome is defined as the ratio of the number of distinct substrings to the total number of substrings of a given length. For a genome of length n, we consider all substrings of length k, and compute the ratio of distinct substrings to total substrings.

## Solution in JavaScript

```javascript
function linguisticComplexity(genome, k) {
    // Handle edge cases
    if (genome.length < k || k <= 0) {
        return 0;
    }
    
    // Create a map to count occurrences of each substring
    const substringCount = new Map();
    
    // Extract all substrings of length k
    for (let i = 0; i <= genome.length - k; i++) {
        const substring = genome.substring(i, i + k);
        substringCount.set(substring, (substringCount.get(substring) || 0) + 1);
    }
    
    // Calculate total number of substrings of length k
    const totalSubstrings = genome.length - k + 1;
    
    // Calculate number of distinct substrings
    const distinctSubstrings = substringCount.size;
    
    // Return the linguistic complexity
    return distinctSubstrings / totalSubstrings;
}

// Alternative implementation using Set for distinct substrings
function linguisticComplexityAlt(genome, k) {
    if (genome.length < k || k <= 0) {
        return 0;
    }
    
    const distinctSubstrings = new Set();
    const totalSubstrings = genome.length - k + 1;
    
    for (let i = 0; i <= genome.length - k; i++) {
        distinctSubstrings.add(genome.substring(i, i + k));
    }
    
    return distinctSubstrings.size / totalSubstrings;
}

// Example usage:
const genome = "ACGTACGT";
const k = 2;

console.log(`Linguistic complexity for genome ${genome} with k=${k}:`);
console.log(linguisticComplexity(genome, k));

// Test with different examples
console.log("\nTest cases:");
console.log("Genome: 'ACGTACGT', k=2:", linguisticComplexity("ACGTACGT", 2));
console.log("Genome: 'AAAAAA', k=2:", linguisticComplexity("AAAAAA", 2));
console.log("Genome: 'ACGTACGT', k=3:", linguisticComplexity("ACGTACGT", 3));
console.log("Genome: 'ATCG', k=1:", linguisticComplexity("ATCG", 1));

// Function to compute linguistic complexity for all k values
function allLinguisticComplexities(genome) {
    const results = {};
    for (let k = 1; k <= genome.length; k++) {
        results[k] = linguisticComplexity(genome, k);
    }
    return results;
}

console.log("\nAll linguistic complexities for genome 'ACGTACGT':");
console.log(allLinguisticComplexities("ACGTACGT"));
```

## Explanation

The solution works as follows:

1. **Edge Case Handling**: First, we check if the genome length is sufficient for the given k value. If not, we return 0.

2. **Substring Extraction**: We iterate through the genome string and extract all substrings of length k.

3. **Counting**: We use a Map to count how many times each substring appears, or alternatively use a Set to collect distinct substrings.

4. **Calculation**: 
   - Total number of substrings of length k = genome.length - k + 1
   - Number of distinct substrings = size of the set/map
   - Linguistic complexity = distinct substrings / total substrings

## Key Features

- **Efficient**: Uses Map for O(1) average lookup time
- **Edge Case Handling**: Properly handles invalid inputs
- **Flexible**: Can compute complexity for any k value
- **Alternative Implementation**: Includes a Set-based approach for comparison

## Time and Space Complexity

- **Time Complexity**: O(n × k) where n is the genome length and k is the substring length
- **Space Complexity**: O(n × k) for storing substrings

The linguistic complexity gives insight into the diversity of patterns within a genome, with higher values indicating greater complexity and diversity of subsequences.

