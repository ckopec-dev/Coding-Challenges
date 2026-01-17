# Rosalind Problem: Compute Limb Lengths in a Tree

## Problem Statement
Given a distance matrix D and a leaf j, compute the limb length for leaf j.

## Solution in Assembly (x86-64)

```assembly
.section .data
    # Distance matrix D (example 4x4 matrix)
    distance_matrix:
        .long 0, 13, 21, 22
        .long 13, 0, 12, 13
        .long 21, 12, 0, 13
        .long 22, 13, 13, 0
    
    # Number of leaves
    num_leaves: .long 4
    
    # Leaf index j
    leaf_j: .long 2

.section .text
    .global _start

compute_limb_length:
    # Function to compute limb length for leaf j
    # Input: 
    #   rdi = pointer to distance matrix
    #   rsi = number of leaves
    #   rdx = leaf index j
    # Output: limb length in rax
    
    push rbp
    mov rbp, rsp
    
    # Get parameters
    mov r8, rdi      # matrix pointer
    mov r9, rsi      # num_leaves
    mov r10, rdx     # leaf j
    
    # Initialize variables
    mov rax, 0       # result (limb length)
    mov rcx, 0       # i counter
    mov r11, 0       # min value
    
    # Outer loop: i = 0 to n-1, i != j
outer_loop:
    cmp rcx, r9      # compare i with num_leaves
    jge outer_loop_end
    
    cmp rcx, r10     # compare i with j
    je inner_loop    # skip if i == j
    
    # Inner loop: k = 0 to n-1, k != j, k != i
    mov r12, 0       # k counter
inner_loop:
    cmp r12, r9      # compare k with num_leaves
    jge inner_loop_end
    
    cmp r12, r10     # compare k with j
    je next_k        # skip if k == j
    
    cmp r12, rcx     # compare k with i
    je next_k        # skip if k == i
    
    # Calculate D[i][j] + D[j][k] - D[i][k]
    # Access matrix elements: D[i][j] = matrix[i*n + j]
    
    # Calculate D[i][j]
    mov r13, rcx     # i
    mov r14, r10     # j
    mov r15, r9      # num_leaves
    imul r13, r15    # i * num_leaves
    add r13, r14     # i * num_leaves + j
    mov r14, r13     # index for D[i][j]
    mov r13, r8      # matrix pointer
    mov r13, [r13 + r14 * 4]  # D[i][j]
    mov r14, r13     # store D[i][j]
    
    # Calculate D[j][k]
    mov r13, r10     # j
    mov r15, r12     # k
    mov r14, r9      # num_leaves
    imul r13, r14    # j * num_leaves
    add r13, r15     # j * num_leaves + k
    mov r15, r13     # index for D[j][k]
    mov r13, r8      # matrix pointer
    mov r13, [r13 + r15 * 4]  # D[j][k]
    mov r15, r13     # store D[j][k]
    
    # Calculate D[i][k]
    mov r13, rcx     # i
    mov r14, r12     # k
    mov r15, r9      # num_leaves
    imul r13, r15    # i * num_leaves
    add r13, r14     # i * num_leaves + k
    mov r14, r13     # index for D[i][k]
    mov r13, r8      # matrix pointer
    mov r13, [r13 + r14 * 4]  # D[i][k]
    mov r14, r13     # store D[i][k]
    
    # Calculate sum = D[i][j] + D[j][k] - D[i][k]
    mov r13, r14     # D[i][k]
    add r13, r15     # D[i][j] + D[j][k]
    sub r13, r10     # D[i][j] + D[j][k] - D[i][k]
    
    # Update minimum
    cmp r11, 0       # if min is 0, set it to first value
    jne check_min
    mov r11, r13     # first value
    jmp next_k
    
check_min:
    cmp r13, r11     # compare with current min
    jg next_k        # if larger, skip
    mov r11, r13     # update min
    
next_k:
    inc r12          # k++
    jmp inner_loop
    
inner_loop_end:
    # Divide by 2 to get limb length
    shr r11, 1       # r11 = (D[i][j] + D[j][k] - D[i][k]) / 2
    
    # Check if this is the minimum
    cmp rax, 0       # if result is 0, set it to first value
    jne check_result
    mov rax, r11     # first value
    jmp next_i
    
check_result:
    cmp r11, rax     # compare with current result
    jg next_i        # if larger, skip
    mov rax, r11     # update result
    
next_i:
    inc rcx          # i++
    jmp outer_loop
    
outer_loop_end:
    mov rax, rax     # return limb length
    
    pop rbp
    ret

# Main function
_start:
    # Set up parameters
    mov rdi, offset distance_matrix  # matrix pointer
    mov rsi, 4                       # num_leaves = 4
    mov rdx, 2                       # leaf j = 2
    
    call compute_limb_length
    
    # Exit program
    mov rax, 60      # sys_exit
    mov rdi, 0       # exit status
    syscall
```

## Algorithm Explanation

The limb length of a leaf j in a tree can be computed using the formula:

```
limb_length(j) = min{ (D[i][j] + D[j][k] - D[i][k]) / 2 }
```

Where the minimum is taken over all pairs of leaves i and k such that i ≠ j and k ≠ j.

## Key Steps:

1. **Initialize**: Set up the distance matrix and parameters
2. **Outer Loop**: Iterate through all leaves i ≠ j
3. **Inner Loop**: For each i, iterate through all leaves k ≠ j, k ≠ i
4. **Calculate**: Compute (D[i][j] + D[j][k] - D[i][k]) / 2
5. **Minimize**: Keep track of the minimum value
6. **Return**: The minimum value is the limb length

## Time Complexity:
O(n³) where n is the number of leaves

## Space Complexity:
O(1) additional space (excluding input matrix)

This assembly implementation follows the mathematical approach to compute limb lengths in a tree, which is essential for the neighbor-joining algorithm in phylogenetic tree construction.

