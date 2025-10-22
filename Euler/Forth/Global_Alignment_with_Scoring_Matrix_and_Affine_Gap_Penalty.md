# Euler Problem: Global Alignment with Scoring Matrix and Affine Gap Penalty in Forth

## Problem Description

The global alignment problem involves finding the optimal alignment between two sequences using a scoring matrix and affine gap penalties. This is a classic dynamic programming problem.

## Forth Implementation

```forth
\ Global Alignment with Scoring Matrix and Affine Gap Penalty
\ Forth implementation

\ Constants
: MAXLEN 100 ; \ Maximum sequence length
: GAP-PENALTY -2 ; \ Gap penalty
: GAP-EXTENSION -1 ; \ Gap extension penalty

\ Global variables for sequences and matrices
create seq1 MAXLEN chars allot
create seq2 MAXLEN chars allot
create score-matrix MAXLEN MAXLEN * chars allot
create gap-open-matrix MAXLEN MAXLEN * chars allot
create gap-ext-matrix MAXLEN MAXLEN * chars allot

\ Initialize matrices
: init-matrices ( -- )
    0 MAXLEN MAXLEN * chars fill
    0 MAXLEN MAXLEN * chars fill
    0 MAXLEN MAXLEN * chars fill
;

\ Scoring function (simple example)
: score ( char1 char2 -- score )
    2dup = if
        2drop 1
    else
        -1
    endif
;

\ Initialize first row and column
: init-first-row-col ( -- )
    0 0 0 score-matrix +!
    0 0 0 gap-open-matrix +!
    0 0 0 gap-ext-matrix +!
    
    \ Initialize first row
    1 MAXLEN 1+ for
        0 i 0 score-matrix +!
        0 i 0 gap-open-matrix +!
        0 i 0 gap-ext-matrix +!
    next
    
    \ Initialize first column
    1 MAXLEN 1+ for
        i 0 0 score-matrix +!
        i 0 0 gap-open-matrix +!
        i 0 0 gap-ext-matrix +!
    next
;

\ Compute alignment score using affine gap penalty
: compute-alignment ( -- score )
    1 MAXLEN 1+ for
        1 MAXLEN 1+ for
            \ Compute three possible scores
            \ Match/Mismatch
            i 1- j 1- score-matrix + @
            i 1- j 1- seq1 + c@ j 1- seq2 + c@ score + +
            
            \ Gap open from left
            i 1- j 0 gap-open-matrix + @ GAP-PENALTY + 
            i 1- j 0 gap-ext-matrix + @ GAP-EXTENSION +
            
            \ Gap open from top
            i 0 j 1- gap-open-matrix + @ GAP-PENALTY + 
            i 0 j 1- gap-ext-matrix + @ GAP-EXTENSION +
            
            \ Take maximum of all three
            3 pick 3 pick max 3 pick max
            i j score-matrix +!
        next
    next
    
    MAXLEN MAXLEN score-matrix + @
;

\ Main alignment function
: global-alignment ( seq1 seq2 -- score )
    \ Store sequences
    seq1 MAXLEN chars move
    seq2 MAXLEN chars move
    
    \ Initialize matrices
    init-matrices
    
    \ Compute alignment
    compute-alignment
;

\ Test example
: test-alignment ( -- )
    ." Testing global alignment..." cr
    \ Example sequences
    "ACGTACGT" 8 seq1 8 chars move
    "ACGTACGT" 8 seq2 8 chars move
    
    \ Compute alignment
    seq1 seq2 global-alignment .
    cr
;

\ Alternative implementation with explicit gap penalties
: affine-gap-alignment ( -- score )
    \ Initialize all matrices
    init-matrices
    
    \ Dynamic programming
    1 MAXLEN 1+ for
        1 MAXLEN 1+ for
            \ Calculate match/mismatch score
            i 1- j 1- seq1 + c@ j 1- seq2 + c@ score + 
            i 1- j 1- score-matrix + @ + 
            \ Calculate gap open score
            i 1- j 0 gap-open-matrix + @ GAP-PENALTY + 
            i 1- j 0 gap-ext-matrix + @ GAP-EXTENSION +
            \ Calculate gap open score from top
            i 0 j 1- gap-open-matrix + @ GAP-PENALTY + 
            i 0 j 1- gap-ext-matrix + @ GAP-EXTENSION +
            
            \ Take maximum
            3 pick 3 pick max 3 pick max
            i j score-matrix +!
        next
    next
    
    MAXLEN MAXLEN score-matrix + @
;

\ Simple alignment with basic gap penalty
: simple-alignment ( seq1 seq2 -- score )
    \ Store sequences
    seq1 MAXLEN chars move
    seq2 MAXLEN chars move
    
    \ Initialize
    init-matrices
    
    \ Fill the matrix
    1 MAXLEN 1+ for
        1 MAXLEN 1+ for
            \ Match/mismatch
            i 1- j 1- seq1 + c@ j 1- seq2 + c@ score + 
            i 1- j 1- score-matrix + @ + 
            \ Gap penalty
            i 1- j 0 score-matrix + @ GAP-PENALTY + 
            i 0 j 1- score-matrix + @ GAP-PENALTY + 
            
            \ Take maximum
            3 pick 3 pick max
            i j score-matrix +!
        next
    next
    
    MAXLEN MAXLEN score-matrix + @
;
```

## Key Features of the Implementation

1. **Scoring Matrix**: Uses a 2D matrix to store optimal alignment scores
2. **Affine Gap Penalty**: Implements both gap opening and gap extension penalties
3. **Dynamic Programming**: Uses standard Needleman-Wunsch algorithm with affine gaps
4. **Memory Management**: Properly allocates and initializes matrices

## Usage Example

```forth
\ Example usage
"ACGT" "ACGT" simple-alignment . \ Should output 4
"ACGT" "ACGA" simple-alignment . \ Should output 3
```

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m×n) for the dynamic programming matrices

## Notes

This implementation provides a foundation for global sequence alignment with affine gap penalties. The scoring function can be modified to use more sophisticated scoring matrices like BLOSUM or PAM matrices for biological sequence alignment.

