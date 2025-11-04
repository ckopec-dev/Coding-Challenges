# Rosalind Problem: Motzkin Numbers and RNA Secondary Structures

## Problem Understanding

The problem asks us to compute the number of possible RNA secondary structures for a given RNA sequence. This is equivalent to computing the nth Motzkin number, where n is the length of the RNA sequence.

In RNA secondary structure prediction, we can form base pairs between complementary nucleotides (A-U, U-A, G-C, C-G) with the constraint that base pairs cannot cross each other (non-crossing). The number of such non-crossing structures follows the Motzkin number sequence.

## Solution Approach

The Motzkin numbers follow this recurrence relation:
- M(0) = 1
- M(n) = M(n-1) + Σ(i=1 to n-1) M(i-1) × M(n-i-1)

However, there's a more efficient approach using the recurrence:
- M(n) = M(n-1) + Σ(i=1 to n-1) M(i-1) × M(n-i-1)

But for RNA structures, we need to consider that:
- M(n) = M(n-1) + Σ(i=1 to n-1) M(i-1) × M(n-i-1) + M(n-2) if n ≥ 2

Actually, the correct recurrence for RNA secondary structures (Motzkin numbers) is:
- M(n) = M(n-1) + Σ(i=1 to n-1) M(i-1) × M(n-i-1)

Let me reconsider - the standard Motzkin number recurrence is:
- M(n) = M(n-1) + Σ(i=1 to n-1) M(i-1) × M(n-i-1)

But for RNA structures with base pairing, we use the recurrence:
- M(n) = M(n-1) + Σ(i=1 to n-1) M(i-1) × M(n-i-1) + M(n-2) if we allow unpaired nucleotides

## Assembly Implementation

```assembly
; Rosalind: Motzkin Numbers and RNA Secondary Structures
; Assembly implementation for computing Motzkin numbers

.data
    ; Precomputed Motzkin numbers for small values
    motzkin_table: .long 1, 1, 2, 4, 9, 21, 51, 127, 323, 835
    ; RNA sequence input (example)
    rna_seq: .ascii "ACGUACGU"
    rna_len: .long 8

.text
.globl _start

_start:
    ; Load RNA sequence length
    movl rna_len, %eax
    
    ; Check if length is small enough for precomputed values
    cmpl $10, %eax
    jge compute_motzkin
    
    ; Use precomputed value
    movl motzkin_table(,%eax,4), %ebx
    jmp done

compute_motzkin:
    ; For larger values, compute using dynamic programming
    ; We'll compute M(n) using bottom-up approach
    pushl %eax          ; Save n
    pushl %ebx          ; Save result
    
    ; Allocate memory for DP table
    movl %eax, %ecx
    addl $1, %ecx       ; M(0) to M(n)
    shll $2, %ecx       ; Multiply by 4 (size of long)
    call malloc
    
    movl %eax, %edi     ; DP table pointer
    
    ; Initialize base cases
    movl $1, (%edi)     ; M(0) = 1
    movl $1, 4(%edi)    ; M(1) = 1
    
    ; Compute Motzkin numbers from 2 to n
    movl $2, %esi       ; i = 2
    movl %eax, %edx     ; n
    
compute_loop:
    cmpl %edx, %esi
    jg compute_done
    
    ; Compute M(i) = M(i-1) + sum of M(j-1) * M(i-1-j) for j from 1 to i-1
    movl $0, %ebx       ; sum = 0
    movl $1, %ecx       ; j = 1
    
inner_loop:
    cmpl %esi, %ecx
    jg inner_done
    
    ; Calculate M(j-1) * M(i-1-j)
    movl %ecx, %eax
    decl %eax           ; j-1
    movl (%edi,%eax,4), %eax
    
    movl %esi, %ebx
    decl %ebx           ; i-1
    subl %ecx, %ebx     ; i-1-j
    movl (%edi,%ebx,4), %ebx
    
    imull %ebx, %eax    ; Multiply
    addl %eax, %edx     ; Add to sum
    
    incl %ecx
    jmp inner_loop
    
inner_done:
    ; M(i) = M(i-1) + sum
    movl -4(%edi,%esi,4), %eax  ; M(i-1)
    addl %edx, %eax              ; Add sum
    movl %eax, (%edi,%esi,4)     ; Store M(i)
    
    incl %esi
    jmp compute_loop

compute_done:
    ; Result is in M(n)
    movl (%edi,%edx,4), %ebx
    
    ; Free memory
    pushl %edi
    call free
    addl $4, %esp
    
done:
    ; Exit program
    movl $1, %eax       ; sys_exit
    movl $0, %ebx       ; exit status
    int $0x80
```

## Alternative Simpler Implementation

```assembly
; Simpler approach for Motzkin numbers
; Using iterative computation

.data
    MAX_N: .long 100
    dp_table: .space 400    ; 100 * 4 bytes

.text
.globl _start

_start:
    ; Initialize DP table
    movl $0, %ecx
    movl $MAX_N, %edx
    
    ; M(0) = 1
    movl $1, dp_table
    
    ; M(1) = 1
    movl $1, 4(dp_table)
    
    ; Compute for i = 2 to n-1
    movl $2, %esi       ; i = 2
    
compute_loop:
    cmpl %edx, %esi
    jge done
    
    ; Compute M(i) = M(i-1) + sum of M(j-1) * M(i-j-1) for j = 1 to i-1
    movl $0, %ebx       ; sum = 0
    movl $1, %edi       ; j = 1
    
inner_loop:
    cmpl %esi, %edi
    jg inner_done
    
    ; Get M(j-1)
    movl %edi, %eax
    decl %eax
    movl dp_table(,%eax,4), %eax
    
    ; Get M(i-j-1)
    movl %esi, %ecx
    subl %edi, %ecx
    decl %ecx
    movl dp_table(,%ecx,4), %ecx
    
    ; Multiply and add to sum
    imull %ecx, %eax
    addl %eax, %ebx
    
    incl %edi
    jmp inner_loop
    
inner_done:
    ; M(i) = M(i-1) + sum
    movl -4(dp_table,%esi,4), %eax  ; M(i-1)
    addl %ebx, %eax                 ; Add sum
    movl %eax, dp_table(,%esi,4)    ; Store result
    
    incl %esi
    jmp compute_loop

done:
    ; Result is in dp_table(n)
    ; Exit
    movl $1, %eax
    movl $0, %ebx
    int $0x80
```

## Key Concepts

1. **Motzkin Numbers**: Count non-crossing partitions of a set
2. **RNA Secondary Structures**: Non-crossing base pairs
3. **Dynamic Programming**: Bottom-up computation of recurrence relation
4. **Memory Management**: Proper allocation and deallocation

## Time and Space Complexity

- **Time Complexity**: O(n²) where n is the length of RNA sequence
- **Space Complexity**: O(n) for the DP table

The solution efficiently computes the number of possible RNA secondary structures using the Motzkin number recurrence relation, which naturally models the non-crossing constraint in RNA base pairing.

