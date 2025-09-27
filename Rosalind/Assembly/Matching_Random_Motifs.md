# Rosalind Problem: Matching Random Motifs

## Problem Description
Given a DNA string s of length at most 100 bp and an array A of 40 floating point numbers representing the probabilities of observing each nucleotide (A, C, G, T) in a random DNA string of the same length, compute the probability that a random DNA string having the same length as s will match s exactly at each position.

## Solution Approach
This is a classic probability problem where we need to calculate the probability of a specific DNA sequence occurring randomly. Since each nucleotide position is independent, we multiply the probabilities of each nucleotide in the given sequence.

## Assembly Implementation

```assembly
.data
    # Input DNA string (max 100 characters)
    dna_string: .ascii "ACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGTACGT"
    dna_length: .long 40
    
    # Array of probabilities for A, C, G, T
    probabilities: .float 0.129, 0.239, 0.345, 0.456, 0.567, 0.678, 0.789, 0.890
                   .float 0.123, 0.234, 0.345, 0.456, 0.567, 0.678, 0.789, 0.890
                   .float 0.123, 0.234, 0.345, 0.456, 0.567, 0.678, 0.789, 0.890
                   .float 0.123, 0.234, 0.345, 0.456, 0.567, 0.678, 0.789, 0.890
                   .float 0.123, 0.234, 0.345, 0.456, 0.567, 0.678, 0.789, 0.890
    
    # Nucleotide mapping: A=0, C=1, G=2, T=3
    nucleotide_map: .byte 0, 1, 2, 3
    
    # Result buffer for output
    result_buffer: .space 20

.text
.globl _start

_start:
    # Initialize registers
    movl dna_length(%esp), %ecx        # Load DNA length into ECX (loop counter)
    xorl %eax, %eax                    # Clear EAX (result accumulator)
    movl $1.0, %edx                    # Initialize probability to 1.0
    
    # Set up pointer to DNA string
    movl $dna_string, %esi             # Load DNA string address into ESI
    
    # Main loop: process each nucleotide
process_loop:
    # Check if we've processed all nucleotides
    testl %ecx, %ecx
    jz calculate_result
    
    # Get current nucleotide character
    movb (%esi), %al                   # Load current character
    
    # Convert character to index (A=0, C=1, G=2, T=3)
    cmpb $'A', %al
    je process_A
    cmpb $'C', %al
    je process_C
    cmpb $'G', %al
    je process_G
    cmpb $'T', %al
    je process_T
    
    # Invalid nucleotide - skip (or handle error)
    jmp next_nucleotide

process_A:
    movl probabilities, %ebx           # Load probability for A (index 0)
    jmp multiply_probability

process_C:
    movl probabilities+4, %ebx         # Load probability for C (index 1)
    jmp multiply_probability

process_G:
    movl probabilities+8, %ebx         # Load probability for G (index 2)
    jmp multiply_probability

process_T:
    movl probabilities+12, %ebx        # Load probability for T (index 3)
    jmp multiply_probability

multiply_probability:
    # Multiply current probability by the nucleotide probability
    # This is a simplified version - in real assembly we'd need floating point operations
    # For demonstration, assuming we have a function to multiply floats
    
    # Call function to multiply probabilities (conceptual)
    call multiply_floats
    
next_nucleotide:
    incl %esi                          # Move to next character
    decl %ecx                          # Decrement counter
    jmp process_loop

calculate_result:
    # Result is now in EDX
    # Convert to string and output
    call float_to_string
    jmp exit_program

# Function to multiply two floating point numbers
multiply_floats:
    pushl %ebp
    movl %esp, %ebp
    
    # This would contain actual floating point multiplication logic
    # For now, we're just showing the concept
    
    movl %ebp, %esp
    popl %ebp
    ret

# Function to convert float to string for output
float_to_string:
    pushl %ebp
    movl %esp, %ebp
    
    # Implementation would convert floating point result to ASCII string
    # This is a placeholder for actual conversion logic
    
    movl %ebp, %esp
    popl %ebp
    ret

exit_program:
    # Exit system call
    movl $1, %eax                      # sys_exit
    movl $0, %ebx                      # exit status
    int $0x80
```

## Optimized Version for Real Implementation

```assembly
.data
    dna_string: .ascii "ACGT"
    dna_length: .long 4
    
    # Precomputed probabilities for A, C, G, T
    prob_A: .float 0.239
    prob_C: .float 0.129
    prob_G: .float 0.345
    prob_T: .float 0.456
    
.text
.globl _start

_start:
    # Initialize floating point unit
    fninit
    
    # Load initial probability (1.0)
    movl $0x3f800000, %eax     # 1.0 in IEEE 754
    movl %eax, %ebx
    fildl %ebx                 # Load as integer and convert to float
    
    # Process each nucleotide
    movl $dna_string, %esi
    movl dna_length(%esp), %ecx
    
process_loop:
    testl %ecx, %ecx
    jz output_result
    
    movb (%esi), %al
    
    # Determine which probability to use
    cmpb $'A', %al
    je use_prob_A
    cmpb $'C', %al
    je use_prob_C
    cmpb $'G', %al
    je use_prob_G
    cmpb $'T', %al
    je use_prob_T
    
use_prob_A:
    flds prob_A(%esp)
    jmp multiply
    
use_prob_C:
    flds prob_C(%esp)
    jmp multiply
    
use_prob_G:
    flds prob_G(%esp)
    jmp multiply
    
use_prob_T:
    flds prob_T(%esp)
    
multiply:
    fmull %st(0), %st(1)       # Multiply current result with new probability
    incl %esi
    decl %ecx
    jmp process_loop

output_result:
    # Store final result (this would need proper formatting)
    fstps (%esp)               # Store result to stack
    
    # Exit program
    movl $1, %eax
    movl $0, %ebx
    int $0x80
```

## Key Points

1. **Probability Calculation**: For each position in the DNA string, multiply the probability of that nucleotide occurring
2. **Floating Point Operations**: Use x87 FPU instructions for floating point multiplication
3. **Character Mapping**: Convert ASCII nucleotides to their corresponding probability indices
4. **Loop Structure**: Process each character sequentially and accumulate the probability

## Time Complexity
- O(n) where n is the length of the DNA string
- Each operation is constant time, so overall linear time complexity

## Space Complexity
- O(1) additional space (excluding input/output)

This implementation assumes the use of x87 floating-point unit instructions for handling decimal probabilities, which is standard in assembly programming for such calculations.

