# Euler Problem: Global Alignment with Scoring Matrix

I'll solve this step-by-step using JavaScript to implement the global sequence alignment algorithm with a scoring matrix.

## Problem Understanding

Global sequence alignment finds the optimal alignment between two sequences using a scoring matrix to determine matches, mismatches, and gaps.

## Solution

```javascript
function globalAlignment(seq1, seq2, scoringMatrix, gapPenalty) {
    const m = seq1.length;
    const n = seq2.length;
    
    // Create scoring matrix
    const score = Array(m + 1).fill().map(() => Array(n + 1).fill(0));
    
    // Initialize first row and column
    for (let i = 0; i <= m; i++) {
        score[i][0] = i * gapPenalty;
    }
    for (let j = 0; j <= n; j++) {
        score[0][j] = j * gapPenalty;
    }
    
    // Fill the scoring matrix
    for (let i = 1; i <= m; i++) {
        for (let j = 1; j <= n; j++) {
            const matchScore = score[i-1][j-1] + scoringMatrix[seq1[i-1]][seq2[j-1]];
            const deleteScore = score[i-1][j] + gapPenalty;
            const insertScore = score[i][j-1] + gapPenalty;
            
            score[i][j] = Math.max(matchScore, deleteScore, insertScore);
        }
    }
    
    // Traceback to find the alignment
    let align1 = '';
    let align2 = '';
    let i = m;
    let j = n;
    
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && 
            score[i][j] === score[i-1][j-1] + scoringMatrix[seq1[i-1]][seq2[j-1]]) {
            align1 = seq1[i-1] + align1;
            align2 = seq2[j-1] + align2;
            i--;
            j--;
        } else if (i > 0 && score[i][j] === score[i-1][j] + gapPenalty) {
            align1 = seq1[i-1] + align1;
            align2 = '-' + align2;
            i--;
        } else {
            align1 = '-' + align1;
            align2 = seq2[j-1] + align2;
            j--;
        }
    }
    
    return {
        score: score[m][n],
        alignment1: align1,
        alignment2: align2
    };
}

// Example usage with DNA sequences
function solveEulerProblem() {
    // Define DNA scoring matrix (match = 2, mismatch = -1)
    const dnaMatrix = {
        'A': {'A': 2, 'T': -1, 'G': -1, 'C': -1},
        'T': {'A': -1, 'T': 2, 'G': -1, 'C': -1},
        'G': {'A': -1, 'T': -1, 'G': 2, 'C': -1},
        'C': {'A': -1, 'T': -1, 'G': -1, 'C': 2}
    };
    
    // Example sequences
    const sequence1 = "ACGTACGT";
    const sequence2 = "ACGTACGT";
    
    const result = globalAlignment(sequence1, sequence2, dnaMatrix, -2);
    
    console.log("Global Alignment Result:");
    console.log("Sequence 1:", sequence1);
    console.log("Sequence 2:", sequence2);
    console.log("Alignment Score:", result.score);
    console.log("Alignment 1:", result.alignment1);
    console.log("Alignment 2:", result.alignment2);
    
    return result;
}

// More complex example with different sequences
function solveComplexExample() {
    // Define a more complex scoring matrix
    const scoringMatrix = {
        'A': {'A': 5, 'T': -1, 'G': -2, 'C': -1},
        'T': {'A': -1, 'T': 5, 'G': -3, 'C': -2},
        'G': {'A': -2, 'T': -3, 'G': 5, 'C': -2},
        'C': {'A': -1, 'T': -2, 'G': -2, 'C': 5}
    };
    
    const seq1 = "ACGTACGT";
    const seq2 = "ACGTACGT";
    
    const result = globalAlignment(seq1, seq2, scoringMatrix, -3);
    
    console.log("\nComplex Example:");
    console.log("Sequence 1:", seq1);
    console.log("Sequence 2:", seq2);
    console.log("Alignment Score:", result.score);
    console.log("Alignment 1:", result.alignment1);
    console.log("Alignment 2:", result.alignment2);
    
    return result;
}

// Function to create a simple scoring matrix for testing
function createSimpleScoringMatrix(matchScore = 2, mismatchScore = -1, gapScore = -1) {
    const matrix = {};
    const bases = ['A', 'T', 'G', 'C'];
    
    bases.forEach(base1 => {
        matrix[base1] = {};
        bases.forEach(base2 => {
            matrix[base1][base2] = (base1 === base2) ? matchScore : mismatchScore;
        });
    });
    
    return matrix;
}

// Run the examples
console.log("=== Euler Problem: Global Alignment ===");
const simpleResult = solveEulerProblem();
const complexResult = solveComplexExample();

// Test with different sequences to show gap penalties
function testGapPenalties() {
    const matrix = createSimpleScoringMatrix(2, -1, -2);
    const seq1 = "ACGT";
    const seq2 = "ACG";
    
    const result = globalAlignment(seq1, seq2, matrix, -2);
    
    console.log("\nGap Penalty Test:");
    console.log("Sequence 1:", seq1);
    console.log("Sequence 2:", seq2);
    console.log("Alignment Score:", result.score);
    console.log("Alignment 1:", result.alignment1);
    console.log("Alignment 2:", result.alignment2);
}

testGapPenalties();
```

## Explanation

This solution implements the global sequence alignment algorithm using dynamic programming:

1. **Scoring Matrix Initialization**: Creates a matrix to store alignment scores
2. **Base Cases**: Initializes first row and column with gap penalties
3. **Matrix Filling**: Uses the recurrence relation:
   ```
   score[i][j] = max(
       score[i-1][j-1] + match_score,  // match/mismatch
       score[i-1][j] + gap_penalty,    // deletion
       score[i][j-1] + gap_penalty     // insertion
   )
   ```

4. **Traceback**: Reconstructs the optimal alignment by backtracking through the matrix

## Key Features

- **Flexible Scoring**: Accepts custom scoring matrices
- **Gap Penalties**: Handles gap penalties for insertions and deletions
- **Optimal Alignment**: Finds the globally optimal alignment
- **Traceback**: Returns both the score and the actual aligned sequences

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m×n) for the scoring matrix

This implementation efficiently solves the global alignment problem with customizable scoring matrices, making it suitable for various bioinformatics applications.

