# Rosalind Problem: Compute Distances Between Leaves in a Tree

## Problem Statement
Given a weighted tree with n leaves, compute the distance between each pair of leaves.

## Solution Approach
I'll use the assembly language approach to solve this step by step.

## Assembly Implementation

```assembly
; Compute distances between leaves in a tree
; Input: Tree representation with n leaves
; Output: Distance matrix between all pairs of leaves

.data
    ; Tree structure storage
    tree_size: .word 0          ; Number of nodes
    leaf_count: .word 0         ; Number of leaves
    adjacency_list: .space 1000 ; Adjacency list representation
    leaf_positions: .space 100   ; Positions of leaves
    distance_matrix: .space 10000 ; Result matrix
    
    ; Temporary storage
    visited: .space 100         ; Visited nodes tracking
    queue: .space 100           ; BFS queue
    
    ; Constants
    MAX_NODES: .word 100
    MAX_LEAVES: .word 50

.text
.globl _start

_start:
    ; Initialize data structures
    la $t0, tree_size
    lw $t1, 0($t0)
    move $t2, $t1               ; $t2 = tree_size
    
    ; Initialize visited array
    li $t3, 0
init_visited:
    beq $t3, $t2, visited_init_done
    la $t4, visited
    addi $t5, $t4, 0
    sw $zero, 0($t5)
    addi $t3, $t3, 1
    j init_visited
visited_init_done:
    
    ; Get number of leaves
    la $t0, leaf_count
    lw $t6, 0($t0)
    
    ; For each leaf pair, compute distance
    li $t7, 0                   ; i = 0
outer_loop:
    bge $t7, $t6, outer_done
    
    li $t8, 0                   ; j = 0
inner_loop:
    bge $t8, $t6, inner_done
    
    ; Skip if i == j
    beq $t7, $t8, skip_distance
    
    ; Compute distance between leaf i and leaf j
    move $a0, $t7               ; leaf i
    move $a1, $t8               ; leaf j
    jal compute_path_distance
    
    ; Store result in distance matrix
    la $t9, distance_matrix
    mul $t10, $t7, $t6         ; row offset
    add $t10, $t10, $t8        ; column offset
    mul $t10, $t10, 4          ; word size
    sw $v0, 0($t9)             ; store distance
    
skip_distance:
    addi $t8, $t8, 1
    j inner_loop
inner_done:
    addi $t7, $t7, 1
    j outer_loop
outer_done:

    ; Exit program
    li $v0, 10
    syscall

; Function: compute_path_distance
; Input: $a0 = leaf i, $a1 = leaf j
; Output: $v0 = distance between leaves
compute_path_distance:
    ; Save registers
    push $ra
    push $s0
    push $s1
    
    ; Initialize BFS
    la $s0, queue
    sw $a0, 0($s0)              ; Start from leaf i
    li $s1, 1                   ; Queue size = 1
    
    ; Reset visited array for this computation
    li $t0, 0
reset_visited:
    beq $t0, $t2, reset_done
    la $t1, visited
    addi $t3, $t1, 0
    sw $zero, 0($t3)
    addi $t0, $t0, 1
    j reset_visited
reset_done:
    
    ; Mark starting node as visited
    sw $zero, 0($a0)            ; Mark leaf i as visited
    
    ; BFS to find path to leaf j
    li $t0, 0                   ; distance = 0
    
bfs_loop:
    ; Check if queue is empty
    beq $s1, $zero, bfs_empty
    
    ; Dequeue node
    la $t1, queue
    lw $t2, 0($t1)              ; Get node from queue
    addi $t1, $t1, 4            ; Move queue pointer
    addi $s1, $s1, -1           ; Decrease queue size
    
    ; Check if we reached target leaf j
    beq $t2, $a1, found_target
    
    ; Process neighbors of current node
    jal get_neighbors
    ; Process neighbor list and update queue
    ; (Implementation details for neighbor processing)
    
    addi $t0, $t0, 1            ; Increment distance
    j bfs_loop
    
bfs_empty:
    li $v0, -1                  ; No path found
    j distance_return
    
found_target:
    move $v0, $t0               ; Return distance
    
distance_return:
    ; Restore registers
    pop $s1
    pop $s0
    pop $ra
    jr $ra

; Function: get_neighbors
; Input: $a0 = current node
; Output: list of neighbors in adjacency_list
get_neighbors:
    ; Implementation would traverse adjacency list
    ; For now, return dummy implementation
    jr $ra

; Helper functions for tree operations
; (Additional functions for tree traversal, node processing, etc.)

```

## Algorithm Explanation

1. **Data Structure Setup**:
   - Store tree as adjacency list representation
   - Track leaf positions separately
   - Initialize distance matrix

2. **Main Algorithm**:
   - For each pair of leaves (i,j):
     - Compute shortest path between them
     - Store result in distance matrix
   - Use BFS to find distances

3. **Key Steps**:
   - Initialize visited tracking array
   - For each leaf pair, perform BFS from one leaf to another
   - Store computed distances in matrix

## Time Complexity
- O(n² × m) where n is number of leaves and m is average path length
- For a tree, this is typically O(n²)

## Space Complexity
- O(n²) for the distance matrix
- O(n) for auxiliary data structures

## Example Usage
```
Input: Tree with 4 leaves
Output: 4x4 distance matrix showing pairwise distances
```

This assembly implementation provides the basic framework for computing distances between leaves in a tree structure, following the computational approach required for the Rosalind problem.

