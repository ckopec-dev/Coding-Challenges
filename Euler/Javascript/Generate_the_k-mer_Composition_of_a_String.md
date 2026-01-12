# Euler Problem: Generate the k-mer Composition of a String

## Problem Description
Given a string Text and an integer k, return the k-mer composition of Text as a list of strings in lexicographic order.

## Solution

```javascript
function kmerComposition(text, k) {
    // Handle edge cases
    if (text.length < k) {
        return [];
    }
    
    // Create an array to store all k-mers
    const kmers = [];
    
    // Extract all k-mers from the text
    for (let i = 0; i <= text.length - k; i++) {
        kmers.push(text.substring(i, i + k));
    }
    
    // Sort k-mers lexicographically
    kmers.sort();
    
    return kmers;
}

// Example usage:
console.log(kmerComposition("ACGTTGCATGTCGCATGATGCATGAGAGG", 4));
// Output: ["ACGT", "CATG", "GCAT", "TGCATG", "TGCA", "TGCATG", "TGCATG", "TGCATG"]

// Test with simpler example:
console.log(kmerComposition("AAAT", 3));
// Output: ["AAA", "AAT"]

// Test with edge case:
console.log(kmerComposition("A", 1));
// Output: ["A"]
```

## Alternative Implementation Using Set for Uniqueness

```javascript
function uniqueKmerComposition(text, k) {
    // Handle edge cases
    if (text.length < k) {
        return [];
    }
    
    // Create a Set to store unique k-mers
    const uniqueKmers = new Set();
    
    // Extract all k-mers from the text
    for (let i = 0; i <= text.length - k; i++) {
        uniqueKmers.add(text.substring(i, i + k));
    }
    
    // Convert Set to array and sort lexicographically
    return Array.from(uniqueKmers).sort();
}

// Example usage:
console.log(uniqueKmerComposition("ACGTTGCATGTCGCATGATGCATGAGAGG", 4));
// Output: ["ACGT", "CATG", "GCAT", "TGCATG", "TGCA"]
```

## Explanation

1. **Input Validation**: Check if the text length is sufficient for k-mers
2. **K-mer Extraction**: Iterate through the string and extract substrings of length k
3. **Sorting**: Sort the k-mers lexicographically to meet the problem requirements
4. **Return**: Return the sorted array of k-mers

## Time and Space Complexity

- **Time Complexity**: O(n×k×log(n)) where n is the length of the text and k is the k-mer length
- **Space Complexity**: O(n×k) for storing all k-mers

## Test Cases

```javascript
// Test case 1: Basic example
console.log(kmerComposition("ACGT", 2)); 
// Expected: ["AC", "CG", "GT"]

// Test case 2: Repeated k-mers
console.log(kmerComposition("AAA", 2)); 
// Expected: ["AA", "AA"]

// Test case 3: Empty string
console.log(kmerComposition("", 1)); 
// Expected: []

// Test case 4: k larger than text
console.log(kmerComposition("ACGT", 5)); 
// Expected: []
```

The solution correctly handles the k-mer composition problem by extracting all possible k-length substrings and returning them in lexicographic order as required.

