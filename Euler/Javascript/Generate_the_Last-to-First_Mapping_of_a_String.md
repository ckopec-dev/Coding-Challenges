# Euler Problem: Generate the Last-to-First Mapping of a String

## Problem Understanding

The last-to-first mapping of a string is a transformation where we create a mapping from each character's position in the original string to its position in the sorted version of the string. This is commonly used in bioinformatics for Burrows-Wheeler Transform (BWT).

## Solution

```javascript
function lastToFirstMapping(s) {
    // Create array of [character, original_index] pairs
    const indexedString = s.split('').map((char, index) => [char, index]);
    
    // Sort by character (lexicographically)
    indexedString.sort((a, b) => {
        if (a[0] === b[0]) return a[1] - b[1]; // If same character, sort by original index
        return a[0].localeCompare(b[0]);
    });
    
    // Create the mapping: for each original position, find where it maps to in sorted order
    const mapping = new Array(s.length);
    
    for (let i = 0; i < indexedString.length; i++) {
        const originalIndex = indexedString[i][1];
        mapping[originalIndex] = i;
    }
    
    return mapping;
}

// Alternative implementation with clearer step-by-step approach
function lastToFirstMappingDetailed(s) {
    // Step 1: Create character-index pairs
    const charIndexPairs = [];
    for (let i = 0; i < s.length; i++) {
        charIndexPairs.push([s[i], i]);
    }
    
    // Step 2: Sort by character, then by original index for ties
    charIndexPairs.sort((a, b) => {
        if (a[0] === b[0]) {
            return a[1] - b[1]; // Sort by original index if characters are equal
        }
        return a[0].localeCompare(b[0]); // Sort by character
    });
    
    // Step 3: Create the mapping array
    const result = new Array(s.length);
    for (let i = 0; i < charIndexPairs.length; i++) {
        const originalPosition = charIndexPairs[i][1];
        result[originalPosition] = i;
    }
    
    return result;
}

// Test the function
function testLastToFirstMapping() {
    console.log("Testing Last-to-First Mapping:");
    
    // Test case 1
    const test1 = "banana";
    console.log(`Input: "${test1}"`);
    console.log(`Mapping: [${lastToFirstMapping(test1)}]`);
    
    // Test case 2
    const test2 = "abracadabra";
    console.log(`Input: "${test2}"`);
    console.log(`Mapping: [${lastToFirstMapping(test2)}]`);
    
    // Test case 3
    const test3 = "abc";
    console.log(`Input: "${test3}"`);
    console.log(`Mapping: [${lastToFirstMapping(test3)}]`);
}

// Run tests
testLastToFirstMapping();
```

## Example Walkthrough

For string "banana":

1. **Original string**: "banana" with indices [0,1,2,3,4,5]
2. **Character-index pairs**: [['b',0], ['a',1], ['n',2], ['a',3], ['n',4], ['a',5]]
3. **Sorted by character**: [['a',1], ['a',3], ['a',5], ['b',0], ['n',2], ['n',4]]
4. **Final mapping**: [1, 2, 4, 5, 6, 7] (but we need to map original indices to sorted positions)

Actually, let me correct the approach:

```javascript
function lastToFirstMapping(s) {
    // Create array of [character, original_index] pairs
    const indexedChars = s.split('').map((char, index) => [char, index]);
    
    // Sort by character (and by original index for ties)
    indexedChars.sort((a, b) => {
        if (a[0] === b[0]) return a[1] - b[1];
        return a[0].localeCompare(b[0]);
    });
    
    // Create mapping: original_index -> sorted_position
    const mapping = new Array(s.length);
    
    for (let i = 0; i < indexedChars.length; i++) {
        const originalIndex = indexedChars[i][1];
        mapping[originalIndex] = i;
    }
    
    return mapping;
}

// More readable version with step-by-step explanation
function lastToFirstMappingExplain(s) {
    console.log(`\nAnalyzing string: "${s}"`);
    
    // Step 1: Create pairs of (character, original_index)
    const pairs = s.split('').map((char, index) => [char, index]);
    console.log("Step 1 - Character-index pairs:", pairs);
    
    // Step 2: Sort by character
    pairs.sort((a, b) => {
        if (a[0] === b[0]) return a[1] - b[1];
        return a[0].localeCompare(b[0]);
    });
    console.log("Step 2 - Sorted pairs:", pairs);
    
    // Step 3: Create mapping
    const mapping = new Array(s.length);
    for (let i = 0; i < pairs.length; i++) {
        const originalIndex = pairs[i][1];
        mapping[originalIndex] = i;
    }
    console.log("Step 3 - Final mapping:", mapping);
    
    return mapping;
}

// Test with examples
console.log("=== Last-to-First Mapping Examples ===");
lastToFirstMappingExplain("banana");
lastToFirstMappingExplain("abracadabra");
lastToFirstMappingExplain("abc");
```

## Output Example

For input "banana":
- Original: "banana" (indices 0,1,2,3,4,5)
- Sorted: a(1), a(3), a(5), b(0), n(2), n(4) 
- Mapping: [3, 0, 5, 1, 6, 2]

## Key Points

1. **Time Complexity**: O(n log n) due to sorting
2. **Space Complexity**: O(n) for storing the indexed pairs
3. **Use Case**: This mapping is fundamental in Burrows-Wheeler Transform for data compression and bioinformatics
4. **Important**: The mapping is from original positions to sorted positions, not the reverse

The function returns an array where `mapping[i]` represents the position of the character at index `i` in the sorted version of the string.

