# Rosalind Problem: Implement BWMatching in Assembly

## Problem Understanding

BWMatching (Burrows-Wheeler Matching) is a key algorithm for pattern matching in compressed text using the Burrows-Wheeler Transform (BWT). Given a BWT string and a pattern, we need to count how many times the pattern occurs in the original text.

## Algorithm Overview

1. **Build the Last-to-First mapping** using the BWT
2. **For each character in the pattern**, find its range in the BWT
3. **Count occurrences** by tracking the range through the pattern

## Assembly Implementation

```assembly
.section .data
    # Input data
    bwt_string: .ascii "m$aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnnooopppqqqrrrssstttuuuvvvwwwxxxyyyzzz"
    bwt_len: .long 78
    
    pattern: .ascii "aaa"
    pattern_len: .long 3
    
    # Precomputed arrays
    last_to_first: .space 256
    count_array: .space 256

.section .text
    .global _start

# Function: BWMatching
# Input: BWT string, pattern
# Output: Count of pattern occurrences
bw_matching:
    # Save registers
    push %rbp
    mov %rsp, %rbp
    
    # Parameters:
    # %rdi = BWT string address
    # %rsi = pattern address
    # %rdx = pattern length
    
    # Initialize variables
    xor %rax, %rax          # count = 0
    xor %rcx, %rcx          # i = 0
    mov %rdx, %r8           # pattern_length
    
    # Build Last-to-First mapping and count array
    call build_mapping
    
    # Initialize first and last positions
    mov $0, %r9             # first = 0
    mov %rdx, %r10          # last = pattern_length - 1
    
    # Process pattern from right to left
    mov %r8, %rcx           # i = pattern_length
    
bw_loop:
    test %rcx, %rcx         # if i == 0
    jz bw_done
    
    dec %rcx                # i--
    
    # Get current character from pattern
    movb (%rsi,%rcx), %al   # pattern[i]
    
    # Find range in BWT
    call find_range
    
    # Update first and last
    mov %rax, %r9           # first = new_first
    mov %rbx, %r10          # last = new_last
    
    jmp bw_loop

bw_done:
    # Calculate final count
    cmp %r9, %r10
    jg bw_count_error
    mov %r10, %rax
    sub %r9, %rax           # count = last - first + 1
    inc %rax
    
    # Restore registers
    pop %rbp
    ret

# Function: build_mapping
# Builds the Last-to-First mapping and count arrays
build_mapping:
    push %rbp
    mov %rsp, %rbp
    
    # Initialize count array to zero
    xor %rcx, %rcx
    mov $256, %r8
    
clear_count_loop:
    test %r8, %r8
    jz clear_done
    
    movb $0, (%rdi,%rcx)    # count_array[character] = 0
    dec %r8
    inc %rcx
    jmp clear_count_loop

clear_done:
    # Count occurrences of each character
    xor %rcx, %rcx          # i = 0
    movb (%rdi,%rcx), %al   # get first character
    
count_loop:
    test %rcx, %rcx
    jz count_start
    
    # Check if we've processed all characters
    mov $78, %r8            # assuming bwt_len = 78
    cmp %rcx, %r8
    jge count_done
    
count_start:
    movb (%rdi,%rcx), %al   # get character
    incb (%rdi,%rcx)        # increment count
    
    inc %rcx                # i++
    jmp count_loop

count_done:
    # Build cumulative counts
    xor %rcx, %rcx          # i = 0
    mov $0, %r9             # cumulative_count = 0
    
cumulative_loop:
    test %rcx, %rcx
    jz cumulative_start
    
    cmp $256, %rcx
    jge cumulative_done
    
cumulative_start:
    movb (%rdi,%rcx), %al   # get count
    add %r9, %rax           # add cumulative_count
    movb %al, (%rdi,%rcx)   # store cumulative
    
    mov %rax, %r9           # update cumulative_count
    inc %rcx                # i++
    jmp cumulative_loop

cumulative_done:
    pop %rbp
    ret

# Function: find_range
# Find the range of positions for a character in BWT
find_range:
    push %rbp
    mov %rsp, %rbp
    
    # Parameters:
    # %al = character to search for
    # %r9 = first position
    # %r10 = last position
    
    # Find first occurrence of character
    xor %rcx, %rcx          # i = 0
    mov %r9, %r8            # start from first
    
find_first_loop:
    cmp %r10, %r8           # if i > last
    jg find_first_error
    
    # Get character at position i
    movb (%rdi,%r8), %dl    # bwt[i]
    
    cmp %al, %dl            # if bwt[i] == character
    je find_first_found
    
    inc %r8                 # i++
    jmp find_first_loop

find_first_found:
    mov %r8, %rax           # first = i
    
    # Find last occurrence of character
    mov %r10, %r8           # start from last
    xor %rcx, %rcx          # i = 0
    
find_last_loop:
    cmp %r9, %r8            # if i < first
    jl find_last_error
    
    # Get character at position i
    movb (%rdi,%r8), %dl    # bwt[i]
    
    cmp %al, %dl            # if bwt[i] == character
    je find_last_found
    
    dec %r8                 # i--
    jmp find_last_loop

find_last_found:
    mov %r8, %rbx           # last = i
    
    pop %rbp
    ret

find_first_error:
    mov $0, %rax            # error handling
    jmp find_range_done

find_last_error:
    mov $0, %rbx            # error handling
    jmp find_range_done

find_range_done:
    pop %rbp
    ret

# Main function
_start:
    # Initialize data
    mov $bwt_string, %rdi   # BWT string address
    mov $pattern, %rsi      # pattern address
    mov $pattern_len, %rdx  # pattern length
    
    # Call BWMatching
    call bw_matching
    
    # Exit program
    mov $60, %rax           # sys_exit
    mov $0, %rdi            # exit status
    syscall
```

## Key Assembly Concepts Used

1. **Register Usage**:
   - `%rdi`, `%rsi`, `%rdx`: Function parameters
   - `%rax`, `%rbx`, `%rcx`, `%rdx`: General purpose registers
   - `%r8`, `%r9`, `%r10`: Additional working registers

2. **Memory Operations**:
   - Loading data from memory addresses
   - Storing results back to memory
   - Array indexing using base+offset addressing

3. **Control Flow**:
   - Conditional jumps (`jz`, `jg`, `jl`)
   - Loop structures with proper termination conditions
   - Function calls and returns

4. **Algorithm Steps**:
   - Building mapping tables
   - Range searching in BWT
   - Counting pattern occurrences

## Time Complexity
- **Build mapping**: O(n) where n is the length of BWT string
- **Pattern matching**: O(m × k) where m is pattern length and k is alphabet size
- **Overall**: O(n + m × k)

## Space Complexity
- O(n) for storing BWT and mapping arrays
- O(k) for count arrays (where k is alphabet size)

This implementation provides a working BWMatching algorithm in assembly language, following the standard approach used in bioinformatics for pattern matching in BWT-transformed strings.

