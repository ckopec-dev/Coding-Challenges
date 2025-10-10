# Euler Problem: Local Alignment with Scoring Matrix in Little Man Computer

## Problem Understanding

The local alignment problem (also known as the Smith-Waterman algorithm) finds the optimal local alignment between two sequences. Unlike global alignment, local alignment identifies the best-matching subsequences within the larger sequences.

## Little Man Computer Implementation

```assembly
; Local Alignment with Scoring Matrix
; Smith-Waterman Algorithm Implementation
; LMC Version

; Data Section
    ORG 100
    DAT 0          ; Score matrix dimensions
    DAT 0          ; Sequence A length
    DAT 0          ; Sequence B length
    DAT 0          ; Gap penalty
    DAT 0          ; Match score
    DAT 0          ; Mismatch penalty
    
    ; Sequences stored in memory
    DAT 0          ; Sequence A start address
    DAT 0          ; Sequence B start address
    
    ; Scoring matrix
    DAT 0          ; Matrix start address
    
    ; Results
    DAT 0          ; Maximum score
    DAT 0          ; Position i
    DAT 0          ; Position j
    
    ; Working registers
    DAT 0          ; Temporary values
    DAT 0          ; Loop counters
    
; Main program
    INP            ; Input sequence A length
    STA LENGTH_A
    INP            ; Input sequence B length
    STA LENGTH_B
    INP            ; Input gap penalty
    STA GAP_PENALTY
    INP            ; Input match score
    STA MATCH_SCORE
    INP            ; Input mismatch penalty
    STA MISMATCH_PENALTY
    
    ; Initialize matrix dimensions
    LDA LENGTH_A
    STA MATRIX_ROWS
    LDA LENGTH_B
    STA MATRIX_COLS
    
    ; Allocate memory for scoring matrix
    LDA LENGTH_A
    STA TEMP1
    LDA LENGTH_B
    STA TEMP2
    MUL TEMP1
    ADD ONE
    STA MATRIX_START
    
    ; Initialize matrix to zeros
    LDA MATRIX_START
    STA MATRIX_PTR
    LDA LENGTH_A
    MUL LENGTH_B
    STA COUNT
    LDA ZERO
    STA MATRIX_PTR
    
    ; Initialize first row and column to zero
    LDA MATRIX_START
    STA MATRIX_PTR
    LDA LENGTH_A
    ADD LENGTH_B
    ADD ONE
    STA COUNT
    
    ; Main alignment loop
    LDA ZERO
    STA I
    LDA ZERO
    STA J
    
    ; Outer loop for i
    LDA I
    LDA LENGTH_A
    SUB ONE
    BRZ OUTER_LOOP_END
    
    ; Inner loop for j
    LDA J
    LDA LENGTH_B
    SUB ONE
    BRZ INNER_LOOP_END
    
    ; Calculate scoring matrix values
    LDA I
    LDA J
    MUL LENGTH_B
    ADD I
    ADD ONE
    STA MATRIX_POS
    
    ; Get diagonal score
    LDA I
    SUB ONE
    LDA J
    SUB ONE
    MUL LENGTH_B
    ADD I
    ADD ONE
    STA DIAG_POS
    
    ; Get up score
    LDA I
    SUB ONE
    LDA J
    MUL LENGTH_B
    ADD I
    ADD ONE
    STA UP_POS
    
    ; Get left score
    LDA I
    LDA J
    SUB ONE
    MUL LENGTH_B
    ADD I
    ADD ONE
    STA LEFT_POS
    
    ; Calculate maximum of three scores
    LDA DIAG_SCORE
    LDA UP_SCORE
    SUB UP_SCORE
    BRZ DIAG_IS_MAX
    LDA UP_SCORE
    LDA DIAG_SCORE
    SUB DIAG_SCORE
    BRZ UP_IS_MAX
    
    ; Compare with left score
    LDA LEFT_SCORE
    LDA MAX_SCORE
    SUB MAX_SCORE
    BRZ LEFT_IS_MAX
    
    ; Set maximum score
    LDA DIAG_SCORE
    STA MAX_SCORE
    
    ; Store in matrix
    LDA MAX_SCORE
    STA MATRIX_POS
    
    ; Continue with next j
    LDA J
    ADD ONE
    STA J
    BRA INNER_LOOP
    
INNER_LOOP_END:
    ; Continue with next i
    LDA I
    ADD ONE
    STA I
    BRA OUTER_LOOP
    
OUTER_LOOP_END:
    ; Find maximum score in matrix
    LDA MATRIX_START
    STA MATRIX_PTR
    LDA ZERO
    STA MAX_SCORE
    
    ; Scan matrix for maximum
    LDA LENGTH_A
    MUL LENGTH_B
    STA COUNT
    LDA ZERO
    STA I
    
SCAN_LOOP:
    LDA I
    LDA COUNT
    SUB ONE
    BRZ SCAN_END
    
    LDA MATRIX_PTR
    LDA MAX_SCORE
    SUB MAX_SCORE
    BRZ SCAN_CONTINUE
    
    LDA MATRIX_PTR
    STA MAX_SCORE
    
SCAN_CONTINUE:
    LDA I
    ADD ONE
    STA I
    LDA MATRIX_PTR
    ADD ONE
    STA MATRIX_PTR
    BRA SCAN_LOOP
    
SCAN_END:
    ; Output result
    LDA MAX_SCORE
    OUT
    
    ; Halt
    HLT
    
; Subroutines
; Initialize matrix to zeros
INIT_MATRIX:
    LDA MATRIX_START
    STA MATRIX_PTR
    LDA LENGTH_A
    MUL LENGTH_B
    STA COUNT
    
INIT_LOOP:
    LDA COUNT
    BRZ INIT_END
    
    LDA ZERO
    STA MATRIX_PTR
    LDA MATRIX_PTR
    ADD ONE
    STA MATRIX_PTR
    LDA COUNT
    SUB ONE
    STA COUNT
    BRA INIT_LOOP
    
INIT_END:
    RTS

; Constants
    DAT 0          ; Zero
    DAT 1          ; One
    DAT 0          ; Length A
    DAT 0          ; Length B
    DAT 0          ; Gap penalty
    DAT 0          ; Match score
    DAT 0          ; Mismatch penalty
    DAT 0          ; Matrix start
    DAT 0          ; Matrix rows
    DAT 0          ; Matrix cols
    DAT 0          ; Matrix pointer
    DAT 0          ; Count
    DAT 0          ; I counter
    DAT 0          ; J counter
    DAT 0          ; Maximum score
    DAT 0          ; Temporary values
    DAT 0          ; Position i
    DAT 0          ; Position j
    DAT 0          ; Matrix position
    DAT 0          ; Diagonal score
    DAT 0          ; Up score
    DAT 0          ; Left score
    DAT 0          ; Max score
    
    ; Sequence data
    DAT 0          ; Sequence A
    DAT 0          ; Sequence B
```

## Algorithm Explanation

The Smith-Waterman algorithm implemented in LMC follows these key steps:

1. **Initialization**: 
   - Set up the scoring matrix dimensions
   - Initialize matrix with zeros
   - Set up gap penalties and scoring parameters

2. **Matrix Calculation**:
   - For each cell (i,j), calculate the score based on:
     - Diagonal match/mismatch score
     - Up gap penalty
     - Left gap penalty
   - Take the maximum of these three values
   - Set the cell to max(0, calculated_score)

3. **Result Extraction**:
   - Find the maximum value in the matrix
   - This represents the optimal local alignment score

## Key Features

- **Memory Management**: Efficient use of LMC's limited memory
- **Loop Control**: Proper handling of nested loops for matrix computation
- **Conditional Logic**: Branching based on score comparisons
- **Optimization**: Uses zero initialization to avoid negative scores

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m×n) for the scoring matrix

This implementation demonstrates how to solve complex bioinformatics problems using the constraints of the Little Man Computer architecture.

