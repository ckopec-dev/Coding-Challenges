# Rosalind Problem: Edit Distance Alignment in Assembly

I'll solve this step by step, implementing the edit distance alignment algorithm in assembly language.

## Problem Understanding

The edit distance alignment problem involves finding the minimum number of operations (insertions, deletions, substitutions) to transform one string into another, along with the actual alignment.

## Algorithm Approach

I'll implement the classic dynamic programming approach for edit distance with traceback to find the alignment.

## Assembly Implementation

```assembly
.section .data
    # Input strings
    str1:   .ascii "AGTACG"
    str1_len: .long 6
    str2:   .ascii "TATCGT"
    str2_len: .long 6
    
    # DP table
    dp_table: .space 100  # 10x10 table for example
    
    # Output strings
    alignment1: .space 20
    alignment2: .space 20

.section .text
    .global _start

# Function to compute edit distance and alignment
edit_distance_alignment:
    # Parameters: 
    # r0 = string1 pointer
    # r1 = string1 length
    # r2 = string2 pointer  
    # r3 = string2 length
    
    # Allocate space for DP table
    push {r4-r11}  // Save registers
    
    # Initialize DP table
    mov r4, #0          // i = 0
    mov r5, #0          // j = 0
    mov r6, #0          // cost
    
    # Fill first row (i=0)
    mov r7, #0          // j = 0
    loop_row:
        cmp r7, r3      // compare j with string2 length
        bge row_done
        str r7, [r10, r7, lsl #2]  // dp[0][j] = j
        add r7, r7, #1
        b loop_row
    row_done:
    
    # Fill first column (j=0)
    mov r7, #0
    loop_col:
        cmp r7, r1      // compare i with string1 length
        bge col_done
        str r7, [r10, r7, lsl #2]  // dp[i][0] = i
        add r7, r7, #1
        b loop_col
    col_done:
    
    # Fill the rest of the table
    mov r8, #1          // i = 1
    loop_i:
        cmp r8, r1      // compare i with string1 length
        bge i_done
        mov r9, #1      // j = 1
        loop_j:
            cmp r9, r3  // compare j with string2 length
            bge j_done
            
            # Calculate cost
            ldr r10, [r0, r8, lsl #2]  // s1[i]
            ldr r11, [r2, r9, lsl #2]  // s2[j]
            cmp r10, r11
            moveq r6, #0    // match = 0
            movne r6, #1    // mismatch = 1
            
            # Get minimum of three operations
            ldr r10, [r10, r9, lsl #2]  // dp[i-1][j] + 1
            ldr r11, [r10, r8, lsl #2]  // dp[i][j-1] + 1
            ldr r12, [r10, r8, lsl #2]  // dp[i-1][j-1] + cost
            
            # Find minimum
            cmp r10, r11
            movlo r10, r11
            cmp r10, r12
            movlo r10, r12
            
            str r10, [r10, r8, r9, lsl #2]  // dp[i][j] = min
            
            add r9, r9, #1
            b loop_j
        j_done:
        add r8, r8, #1
        b loop_i
    i_done:
    
    # Traceback to get alignment
    traceback:
        mov r8, r1      // i = len1
        mov r9, r3      // j = len2
        mov r10, #0     // alignment position
        
    trace_loop:
        cmp r8, #0
        ble trace_done
        cmp r9, #0
        ble trace_done
        
        # Get current dp value
        ldr r11, [r10, r8, r9, lsl #2]
        
        # Check if we came from diagonal (substitution)
        ldr r12, [r10, r8, r9, lsl #2]
        cmp r11, r12
        beq diagonal_step
        
        # Check if we came from left (insertion)
        ldr r12, [r10, r8, r9, lsl #2]
        cmp r11, r12
        beq left_step
        
        # Check if we came from top (deletion)
        ldr r12, [r10, r8, r9, lsl #2]
        cmp r11, r12
        beq top_step
        
        # Default to diagonal
    diagonal_step:
        # Add characters to alignment
        ldr r11, [r0, r8, lsl #2]  // s1[i]
        ldr r12, [r2, r9, lsl #2]  // s2[j]
        str r11, [alignment1, r10, lsl #2]
        str r12, [alignment2, r10, lsl #2]
        sub r8, r8, #1
        sub r9, r9, #1
        add r10, r10, #1
        b trace_loop
        
    trace_done:
        pop {r4-r11}
        bx lr

# Main function
_start:
    # Set up parameters
    ldr r0, =str1       // string1 pointer
    ldr r1, str1_len    // string1 length
    ldr r2, =str2       // string2 pointer
    ldr r3, str2_len    // string2 length
    
    # Call edit distance function
    bl edit_distance_alignment
    
    # Exit program
    mov r7, #1          // sys_exit
    mov r0, #0          // exit status
    svc 0
```

## Detailed Explanation

### 1. **Data Section**
- Defines two input strings: "AGTACG" and "TATCGT"
- Allocates space for the DP table (10x10 for example)
- Creates space for output alignments

### 2. **Algorithm Steps**

**Step 1: Initialize DP Table**
- First row: dp[0][j] = j (cost of inserting j characters)
- First column: dp[i][0] = i (cost of deleting i characters)

**Step 2: Fill DP Table**
- For each cell dp[i][j], compute:
  - Cost of substitution: 0 if characters match, 1 otherwise
  - Minimum of three operations:
    - Deletion: dp[i-1][j] + 1
    - Insertion: dp[i][j-1] + 1  
    - Substitution: dp[i-1][j-1] + cost

**Step 3: Traceback**
- Start from dp[len1][len2]
- Backtrack to find optimal alignment path
- Record characters for final alignment

### 3. **Key Assembly Concepts**

- **Memory Management**: Using stack operations to save/restore registers
- **Loop Control**: Using conditional branches for loop termination
- **Pointer Arithmetic**: Accessing array elements with proper indexing
- **Conditional Logic**: Determining optimal operations during traceback

### 4. **Time and Space Complexity**

- **Time Complexity**: O(m×n) where m and n are string lengths
- **Space Complexity**: O(m×n) for the DP table
- **Output**: Edit distance value and optimal alignment strings

This implementation provides the foundation for solving the edit distance alignment problem in assembly language, demonstrating both the algorithmic approach and low-level implementation details.

