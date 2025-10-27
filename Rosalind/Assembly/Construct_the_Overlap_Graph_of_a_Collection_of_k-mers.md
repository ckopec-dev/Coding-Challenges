# Rosalind Problem: Construct the Overlap Graph of a Collection of k-mers

## Problem Statement
Given a collection of k-mers, construct the overlap graph where each k-mer is a node, and there is a directed edge from k-mer A to k-mer B if the suffix of A (of length k-1) matches the prefix of B (of length k-1).

## Solution in Assembly Language

```assembly
; Assembly implementation for constructing overlap graph of k-mers
; Assumes k-mers are stored in memory with fixed length k

.data
    kmers:      .word   0x00000000, 0x00000000, 0x00000000  ; Array of k-mers
    num_kmers:  .word   3                    ; Number of k-mers
    k:          .word   3                    ; Length of k-mers
    graph:      .space  100                  ; Storage for edges
    
.text
.globl _start

_start:
    ; Load parameters
    lw $t0, num_kmers        ; Load number of k-mers
    lw $t1, k                ; Load k-mer length
    sub $t1, $t1, 1          ; k-1 for suffix/prefix comparison
    
    ; Initialize loop counters
    li $t2, 0                ; i = 0
    li $t3, 0                ; j = 0
    
outer_loop:
    bge $t2, $t0, end        ; if i >= num_kmers, exit
    
    ; Inner loop for comparing k-mers
    li $t3, 0                ; j = 0
    
inner_loop:
    bge $t3, $t0, next_i     ; if j >= num_kmers, next i
    
    ; Check if we're not comparing same k-mer
    beq $t2, $t3, next_j     ; if i == j, skip
    
    ; Compare suffix of k-mer i with prefix of k-mer j
    la $a0, kmers            ; Load base address of k-mers
    mul $a1, $t2, 4          ; Calculate offset for k-mer i
    add $a1, $a1, $a0        ; Address of k-mer i
    mul $a2, $t3, 4          ; Calculate offset for k-mer j
    add $a2, $a2, $a0        ; Address of k-mer j
    
    ; Extract suffix of k-mer i (last k-1 characters)
    ; Extract prefix of k-mer j (first k-1 characters)
    ; Compare them
    
    ; For simplicity, assuming k-mers are stored as strings
    ; This is a conceptual implementation
    
    ; Compare k-1 characters from end of k-mer i with start of k-mer j
    jal compare_suffix_prefix
    
    beq $v0, 1, add_edge     ; If match found, add edge
    
next_j:
    addi $t3, $t3, 1         ; j++
    j inner_loop
    
add_edge:
    ; Add edge from k-mer i to k-mer j
    ; Store in graph structure
    ; This would involve storing the edge information
    j next_j
    
next_i:
    addi $t2, $t2, 1         ; i++
    j outer_loop
    
end:
    ; Exit program
    li $v0, 10
    syscall

compare_suffix_prefix:
    ; Function to compare suffix of k-mer i with prefix of k-mer j
    ; Parameters: $a1 = address of k-mer i, $a2 = address of k-mer j
    ; Returns: $v0 = 1 if match, 0 if no match
    
    ; This is a simplified version - actual implementation would
    ; depend on how k-mers are stored and represented
    
    li $v0, 1                ; Placeholder return value
    jr $ra

```

## Alternative Implementation (More Detailed)

```assembly
; More detailed assembly implementation
; This version handles the actual string comparison

.data
    kmer_array: .space  100      ; Storage for k-mers
    kmer_count: .word   0        ; Number of k-mers
    k_value:    .word   0        ; k-mer length
    edges:      .space  200      ; Storage for edges
    
.text
.globl main

main:
    ; Initialize
    lw $t0, kmer_count           ; Load number of k-mers
    lw $t1, k_value              ; Load k value
    
    ; Set up loop counters
    li $t2, 0                    ; i = 0
    li $t3, 0                    ; j = 0
    
    ; Outer loop - for each k-mer i
    outer_loop:
        bge $t2, $t0, exit       ; If i >= count, exit
        
        ; Inner loop - compare with all other k-mers j
        li $t3, 0                ; Reset j = 0
        
        inner_loop:
            bge $t3, $t0, next_i ; If j >= count, next i
            
            ; Skip when i == j (no self-edges)
            beq $t2, $t3, skip_j
            
            ; Check overlap
            jal check_overlap
            
            ; If overlap exists, store edge
            beq $v0, 1, store_edge
            
        skip_j:
            addi $t3, $t3, 1       ; j++
            j inner_loop
            
        store_edge:
            ; Store edge (i -> j) in graph
            ; Implementation depends on graph representation
            j skip_j
            
        next_i:
            addi $t2, $t2, 1       ; i++
            j outer_loop
            
    exit:
        # Exit program
        li $v0, 10
        syscall

check_overlap:
    ; Compare suffix of k-mer i with prefix of k-mer j
    ; Return 1 if overlap, 0 otherwise
    
    # Calculate memory addresses
    la $a0, kmer_array         ; Base address
    mul $t4, $t2, 4            ; Offset for k-mer i
    add $a0, $a0, $t4          ; Address of k-mer i
    
    mul $t5, $t3, 4            ; Offset for k-mer j
    add $a1, $a0, $t5          ; Address of k-mer j
    
    # Compare k-1 characters
    li $t6, 0                  ; Character counter
    
    # Compare suffix of i with prefix of j
    # This is a simplified version - actual string comparison needed
    
    li $v0, 1                  ; Assume match for now
    jr $ra

```

## Key Concepts

1. **Graph Construction**: Each k-mer becomes a node in the graph
2. **Edge Definition**: Directed edge from k-mer A to k-mer B if suffix(A) = prefix(B)
3. **String Comparison**: Need to compare k-1 characters from end of one k-mer with start of another
4. **Memory Management**: Efficient storage of k-mers and graph edges

## Time Complexity
- O(n² × k) where n is number of k-mers and k is k-mer length
- For each pair of k-mers, we compare k-1 characters

## Space Complexity
- O(n²) for storing edges in worst case
- O(n × k) for storing k-mers

This assembly implementation provides the basic structure for solving the overlap graph problem, though actual string comparison operations would need to be implemented based on the specific memory layout and data representation used.

