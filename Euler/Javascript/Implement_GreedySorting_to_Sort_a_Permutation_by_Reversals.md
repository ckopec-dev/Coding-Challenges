# Euler Problem: GreedySorting to Sort a Permutation by Reversals

## Problem Understanding

The GreedySorting algorithm is used to sort a permutation by performing reversals. The goal is to transform a given permutation into the identity permutation (1, 2, 3, ..., n) using the minimum number of reversals.

## Solution

```javascript
function greedySorting(permutation) {
    const reversals = [];
    const n = permutation.length;
    
    // Create a copy to avoid modifying the original array
    const perm = [...permutation];
    
    for (let i = 0; i < n; i++) {
        // Find the position of the element that should be at position i
        let pos = -1;
        for (let j = i; j < n; j++) {
            if (Math.abs(perm[j]) === i + 1) {
                pos = j;
                break;
            }
        }
        
        // If the element is not already in the correct position
        if (pos !== i) {
            // If the element is negative, we need to reverse it first
            if (perm[pos] < 0) {
                // Reverse the segment from i to pos
                reverseSegment(perm, i, pos);
                reversals.push([...perm]);
            }
            
            // Now we need to reverse the segment from i to pos to get the element in position i
            // But first check if we need to reverse the element itself
            if (perm[i] !== i + 1) {
                // Reverse the segment from i to pos
                reverseSegment(perm, i, pos);
                reversals.push([...perm]);
            }
        }
        
        // If the element is negative, we need to flip its sign
        if (perm[i] < 0) {
            perm[i] = -perm[i];
            reversals.push([...perm]);
        }
    }
    
    return reversals;
}

function reverseSegment(perm, start, end) {
    while (start < end) {
        // Flip signs when reversing
        [perm[start], perm[end]] = [-perm[end], -perm[start]];
        start++;
        end--;
    }
}

// Alternative cleaner implementation
function greedySortingClean(permutation) {
    const reversals = [];
    const n = permutation.length;
    const perm = [...permutation];
    
    for (let i = 0; i < n; i++) {
        // Find where the element (i+1) is located
        let pos = -1;
        for (let j = i; j < n; j++) {
            if (Math.abs(perm[j]) === i + 1) {
                pos = j;
                break;
            }
        }
        
        // If element is not in correct position
        if (pos !== i) {
            // If element is negative, flip it first
            if (perm[pos] < 0) {
                // Flip the element to positive
                perm[pos] = -perm[pos];
                reversals.push([...perm]);
            }
            
            // Reverse the segment from i to pos
            reverseSegment(perm, i, pos);
            reversals.push([...perm]);
        }
        
        // If the element is still negative, flip it to positive
        if (perm[i] < 0) {
            perm[i] = -perm[i];
            reversals.push([...perm]);
        }
    }
    
    return reversals;
}

// Even simpler version that matches the typical greedy sorting algorithm
function greedySortingSimple(permutation) {
    const reversals = [];
    const n = permutation.length;
    const perm = [...permutation];
    
    for (let i = 0; i < n; i++) {
        // Find the position of the element (i+1)
        let pos = -1;
        for (let j = i; j < n; j++) {
            if (perm[j] === i + 1) {
                pos = j;
                break;
            }
            if (perm[j] === -(i + 1)) {
                pos = j;
                break;
            }
        }
        
        // If element is not in correct position
        if (pos !== i) {
            // If element is negative, flip it first
            if (perm[pos] < 0) {
                perm[pos] = -perm[pos];
                reversals.push([...perm]);
            }
            
            // Reverse the segment from i to pos
            reverseSegment(perm, i, pos);
            reversals.push([...perm]);
        }
        
        // If element is negative, flip it to positive
        if (perm[i] < 0) {
            perm[i] = -perm[i];
            reversals.push([...perm]);
        }
    }
    
    return reversals;
}

// Most straightforward implementation
function greedySortingFinal(permutation) {
    const reversals = [];
    const n = permutation.length;
    const perm = [...permutation];
    
    for (let i = 0; i < n; i++) {
        // Find the position of the element that should be at position i
        let pos = i;
        while (Math.abs(perm[pos]) !== i + 1) {
            pos++;
        }
        
        // If element is not in correct position
        if (pos !== i) {
            // Reverse the segment from i to pos
            reverseSegment(perm, i, pos);
            reversals.push([...perm]);
        }
        
        // If the element is negative, make it positive
        if (perm[i] < 0) {
            perm[i] = -perm[i];
            reversals.push([...perm]);
        }
    }
    
    return reversals;
}

// Helper function to reverse a segment and flip signs
function reverseSegment(perm, start, end) {
    while (start <= end) {
        // Flip signs when reversing
        [perm[start], perm[end]] = [-perm[end], -perm[start]];
        start++;
        end--;
    }
}

// Test function
function testGreedySorting() {
    // Test case 1
    const perm1 = [3, -1, -2, 4];
    console.log("Input permutation:", perm1);
    const result1 = greedySortingFinal(perm1);
    console.log("Reversals:", result1);
    console.log("Number of reversals:", result1.length);
    
    // Test case 2
    const perm2 = [1, -2, 3, -4];
    console.log("\nInput permutation:", perm2);
    const result2 = greedySortingFinal(perm2);
    console.log("Reversals:", result2);
    console.log("Number of reversals:", result2.length);
}

// Run test
testGreedySorting();
```

## Explanation

The GreedySorting algorithm works as follows:

1. **For each position i from 0 to n-1:**
   - Find where the element (i+1) is located in the permutation
   - If it's not in position i, perform reversals to move it there
   - If the element is negative, flip its sign to make it positive

2. **Key operations:**
   - **Reversal**: Reverse a segment of the permutation and flip signs of elements
   - **Sign flipping**: When an element is negative, we flip its sign to make it positive

3. **Algorithm steps:**
   - Find the position of the element that should be at current position
   - If it's not there, reverse the segment to bring it to the correct position
   - If the element is negative, flip it to positive

## Example

For permutation `[3, -1, -2, 4]`:
1. Position 0 should have 1, but we have 3. Find 1 at position 1.
2. Reverse segment [0,1] to get `[-1, -3, -2, 4]`.
3. Flip sign of first element to get `[1, -3, -2, 4]`.
4. Continue for remaining positions.

The algorithm ensures we sort the permutation using the minimum number of reversals.

