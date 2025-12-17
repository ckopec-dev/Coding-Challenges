# Rosalind Problem: Global Alignment with Scoring Matrix and Affine Gap Penalty

## Problem Description
Given two protein sequences, find the global alignment with the highest score using a scoring matrix and affine gap penalty.

## Solution in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_SEQ_LEN 1000
#define MAX_SCORE 1000000

// Scoring matrix for amino acid alignment
int score_matrix[26][26] = {
    // A   R   N   D   C   Q   E   G   H   I   L   K   M   F   P   S   T   W   Y   V   B   Z   X   *
    { 4, -1, -2, -2,  0, -1, -1,  0, -2, -1, -1, -1, -1, -2, -1,  1,  0, -3, -2,  0, -2, -1, -1, -4}, // A
    {-1,  5,  0, -2, -3,  1,  0, -2,  0, -3, -2,  2, -1, -3, -2, -1, -1, -3, -2, -3, -1,  0, -1, -4}, // R
    {-2,  0,  6,  1, -3,  0,  0,  0,  1, -3, -3,  0, -2, -3, -2, -1, -1, -3, -2, -3,  3,  0, -1, -4}, // N
    {-2, -2,  1,  6, -3,  0,  2, -1, -1, -3, -4, -1, -3, -3, -1, -2, -1, -3, -2, -3,  4,  1, -1, -4}, // D
    { 0, -3, -3, -3,  9, -3, -4, -3, -3, -1, -1, -3, -1, -2, -3, -1, -1, -2, -2, -1, -3, -3, -1, -4}, // C
    {-1,  1,  0,  0, -3,  5,  2, -2,  0, -3, -2,  1,  0, -3, -1,  0, -1, -2, -1, -2,  0,  3, -1, -4}, // Q
    {-1,  0,  0,  2, -4,  2,  5, -2,  0, -3, -3,  1, -2, -3, -1,  0, -1, -3, -2, -2,  1,  4, -1, -4}, // E
    { 0, -2,  0, -1, -3, -2, -2,  6, -2, -4, -4, -2, -3, -3, -2, -2, -2, -4, -3, -3, -2, -2, -1, -4}, // G
    {-2,  0,  1, -1, -3,  0,  0, -2,  8, -3, -3, -1, -2, -1, -2, -1, -2, -2,  2, -3,  0,  0, -1, -4}, // H
    {-1, -3, -3, -3, -1, -3, -3, -4, -3,  4,  2, -3,  1,  0, -3, -2, -1, -1, -1,  3, -3, -3, -1, -4}, // I
    {-1, -2, -3, -4, -1, -2, -3, -4, -3,  2,  4, -2,  2,  0, -3, -2, -1, -1, -1,  1, -4, -3, -1, -4}, // L
    {-1,  2,  0, -1, -3,  1,  1, -2, -1, -3, -2,  5, -1, -3, -1,  0, -1, -3, -2, -2,  0,  1, -1, -4}, // K
    {-1, -1, -2, -3, -1,  0, -2, -3, -2,  1,  2, -1,  5,  0, -2, -1, -1, -1, -1,  1, -3, -1, -1, -4}, // M
    {-2, -3, -3, -3, -2, -3, -3, -3, -1,  0,  0, -3,  0,  6, -4, -2, -2,  1,  3, -1, -3, -3, -1, -4}, // F
    {-1, -2, -2, -1, -3, -1, -1, -2, -2, -3, -3, -1, -2, -4,  7, -1, -1, -4, -3, -2, -2, -1, -1, -4}, // P
    { 1, -1, -1, -2, -1,  0,  0, -2, -1, -2, -2,  0, -1, -2, -1,  4,  1, -3, -2, -1, -1, -1, -1, -4}, // S
    { 0, -1, -1, -1, -1, -1, -1, -2, -2, -1, -1, -1, -1, -2, -1,  1,  5, -3, -2,  0, -1, -1, -1, -4}, // T
    {-3, -3, -3, -3, -2, -2, -3, -4, -2, -1, -1, -3, -1,  1, -4, -3, -3, 12,  2, -1, -3, -2, -1, -4}, // W
    {-2, -2, -2, -2, -2, -1, -2, -3,  2, -1, -1, -2, -1,  3, -3, -2, -2,  2,  7, -1, -2, -1, -1, -4}, // Y
    { 0, -3, -3, -3, -1, -2, -2, -3, -3,  3,  1, -2,  1, -1, -2, -1,  0, -1, -1,  4, -3, -2, -1, -4}, // V
    {-2, -1,  3,  4, -3,  0,  1, -2,  0, -3, -4,  0, -3, -3, -2, -1, -1, -3, -2, -3,  4,  1, -1, -4}, // B
    {-1,  0,  0,  1, -3,  3,  4, -2,  0, -3, -3,  1, -1, -3, -1, -1, -1, -2, -1, -2,  1,  4, -1, -4}, // Z
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -4}, // X
    {-4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4,  1}  // *
};

int gap_open = -11;
int gap_extend = -1;

char seq1[MAX_SEQ_LEN];
char seq2[MAX_SEQ_LEN];
int m, n;

// Function to get amino acid index
int get_aa_index(char aa) {
    if (aa >= 'A' && aa <= 'Z') {
        return aa - 'A';
    } else {
        return 23; // X for unknown amino acid
    }
}

// Function to get score from matrix
int get_score(char aa1, char aa2) {
    int index1 = get_aa_index(aa1);
    int index2 = get_aa_index(aa2);
    return score_matrix[index1][index2];
}

// Function to compute global alignment with affine gap penalty
void compute_alignment() {
    // Create matrices for dynamic programming
    int **F = (int**)malloc((m+1) * sizeof(int*));
    int **I1 = (int**)malloc((m+1) * sizeof(int*));
    int **I2 = (int**)malloc((m+1) * sizeof(int*));
    
    for (int i = 0; i <= m; i++) {
        F[i] = (int*)calloc(n+1, sizeof(int));
        I1[i] = (int*)calloc(n+1, sizeof(int));
        I2[i] = (int*)calloc(n+1, sizeof(int));
    }
    
    // Initialize base cases
    for (int i = 0; i <= m; i++) {
        F[i][0] = i * (gap_open + gap_extend);
        I1[i][0] = i * (gap_open + gap_extend);
        I2[i][0] = -MAX_SCORE;
    }
    
    for (int j = 0; j <= n; j++) {
        F[0][j] = j * (gap_open + gap_extend);
        I1[0][j] = -MAX_SCORE;
        I2[0][j] = j * (gap_open + gap_extend);
    }
    
    // Fill the matrices
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int match = F[i-1][j-1] + get_score(seq1[i-1], seq2[j-1]);
            int gap1 = I1[i-1][j-1] + get_score(seq1[i-1], seq2[j-1]);
            int gap2 = I2[i-1][j-1] + get_score(seq1[i-1], seq2[j-1]);
            
            F[i][j] = fmax(fmax(match, gap1), gap2);
            
            // Compute I1 (gap in sequence 1)
            int open1 = F[i-1][j] + gap_open;
            int extend1 = I1[i-1][j] + gap_extend;
            I1[i][j] = fmax(open1, extend1);
            
            // Compute I2 (gap in sequence 2)
            int open2 = F[i][j-1] + gap_open;
            int extend2 = I2[i][j-1] + gap_extend;
            I2[i][j] = fmax(open2, extend2);
        }
    }
    
    // Print the maximum score
    printf("%d\n", F[m][n]);
    
    // Free memory
    for (int i = 0; i <= m; i++) {
        free(F[i]);
        free(I1[i]);
        free(I2[i]);
    }
    free(F);
    free(I1);
    free(I2);
}

int main() {
    // Read input sequences
    fgets(seq1, MAX_SEQ_LEN, stdin);
    seq1[strcspn(seq1, "\n")] = 0;  // Remove newline
    m = strlen(seq1);
    
    fgets(seq2, MAX_SEQ_LEN, stdin);
    seq2[strcspn(seq2, "\n")] = 0;  // Remove newline
    n = strlen(seq2);
    
    // Compute alignment
    compute_alignment();
    
    return 0;
}
```

## Explanation

This C program solves the global alignment problem with affine gap penalty using dynamic programming:

### Key Components:

1. **Scoring Matrix**: Uses the BLOSUM62 matrix for amino acid substitution scores
2. **Affine Gap Penalty**: Implements different penalties for opening and extending gaps
3. **Three Matrix Approach**: 
   - `F[i][j]`: Score for aligning prefixes of length i and j
   - `I1[i][j]`: Score when gap is opened in sequence 1
   - `I2[i][j]`: Score when gap is opened in sequence 2

### Algorithm Steps:

1. **Initialization**: Set base cases for gaps at boundaries
2. **Recurrence Relations**:
   - For match/mismatch: `F[i][j] = max(F[i-1][j-1] + score, I1[i-1][j-1] + score, I2[i-1][j-1] + score)`
   - For gap in sequence 1: `I1[i][j] = max(F[i-1][j] + gap_open, I1[i-1][j] + gap_extend)`
   - For gap in sequence 2: `I2[i][j] = max(F[i][j-1] + gap_open, I2[i][j-1] + gap_extend)`
3. **Result**: The final score is `F[m][n]`

### Time and Space Complexity:
- **Time**: O(m×n) where m and n are sequence lengths
- **Space**: O(m×n) for the three matrices

### Input Format:
Two protein sequences (strings of amino acids) on separate lines

### Output:
The maximum alignment score with affine gap penalty

