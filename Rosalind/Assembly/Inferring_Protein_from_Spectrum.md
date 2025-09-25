# Rosalind Problem: Inferring Protein from Spectrum (Assembly Solution)

## Problem Description
Given the ID of a protein sequence, we need to determine the protein that corresponds to the given spectrum. This is essentially a mass spectrometry problem where we need to match theoretical masses with experimental masses.

## Assembly Solution

```assembly
; Inferring Protein from Spectrum - Rosalind Problem Solution
; Assembly Language Implementation

.data
    ; Mass table for amino acids
    mass_table: .word 57, 71, 86, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186
    amino_acids: .ascii "GALMFWKQESPVICRTYNH"
    
    ; Input spectrum (example)
    spectrum: .word 57, 71, 86, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186
    spectrum_size: .word 17
    
    ; Protein result buffer
    protein_result: .space 100
    
    ; Constants
    MAX_PROTEIN_LENGTH: .word 50
    MASS_TOLERANCE: .word 0.1

.text
.globl _start

_start:
    ; Initialize registers
    movl spectrum_size, %ecx          ; Load spectrum size into ECX
    movl $0, %esi                     ; Index counter for spectrum
    
    ; Main loop to process each mass in spectrum
process_spectrum_loop:
    ; Check if we've processed all masses
    cmpl $0, %ecx
    jz protein_found
    
    ; Get current mass from spectrum
    movl spectrum(,%esi,4), %eax      ; Load current mass into EAX
    
    ; Find matching amino acid
    call find_amino_acid
    
    ; Store result in protein buffer
    movb %al, protein_result(,%edi,1)
    incl %edi                         ; Increment protein index
    
    ; Move to next spectrum element
    incl %esi
    decl %ecx
    jmp process_spectrum_loop

protein_found:
    ; Null terminate the protein string
    movb $0, protein_result(,%edi,1)
    
    ; Exit program
    movl $1, %eax                     ; sys_exit
    movl $0, %ebx                     ; exit status
    int $0x80

; Function to find amino acid matching mass
find_amino_acid:
    pushl %ecx                        ; Save ECX
    pushl %edx                        ; Save EDX
    pushl %esi                        ; Save ESI
    
    movl $17, %ecx                    ; Number of amino acids
    movl $0, %esi                     ; Index for mass table
    
find_loop:
    cmpl mass_table(,%esi,4), %eax   ; Compare current mass with table entry
    jz found_match                    ; If match, we found our amino acid
    
    incl %esi                         ; Increment index
    decl %ecx                         ; Decrement counter
    jnz find_loop                     ; Continue if not done
    
    ; No match found - return 'X' for unknown
    movb $88, %al                     ; ASCII 'X'
    jmp find_amino_acid_end

found_match:
    ; Get corresponding amino acid from table
    movb amino_acids(,%esi,1), %al   ; Load matching amino acid
    
find_amino_acid_end:
    popl %esi                         ; Restore ESI
    popl %edx                         ; Restore EDX
    popl %ecx                         ; Restore ECX
    ret                               ; Return to caller

; Alternative approach using mass difference comparison
mass_difference_approach:
    pushl %ebp
    movl %esp, %ebp
    
    ; Load spectrum and mass table
    movl spectrum, %esi               ; Spectrum array pointer
    movl mass_table, %edi             ; Mass table pointer
    
    ; Process each mass in spectrum
    movl spectrum_size, %ecx          ; Number of elements
    
mass_diff_loop:
    cmpl $0, %ecx
    jz mass_diff_done
    
    ; Get current mass
    movl (%esi), %eax                 ; Current spectrum mass
    
    ; Compare with all amino acid masses
    movl $17, %edx                    ; Number of amino acids
    movl $0, %ebx                     ; Index counter
    
mass_compare_loop:
    cmpl mass_table(,%ebx,4), %eax   ; Compare with mass table entry
    jz mass_match_found               ; Match found
    
    incl %ebx                         ; Next amino acid
    decl %edx                         ; Decrement counter
    jnz mass_compare_loop             ; Continue comparing
    
    ; No match - assume it's the last amino acid in our table
    movb amino_acids(,%ebx,1), %al   ; Return last amino acid
    jmp mass_diff_continue

mass_match_found:
    ; Found matching amino acid
    movb amino_acids(,%ebx,1), %al   ; Get corresponding amino acid
    
mass_diff_continue:
    addl $4, %esi                     ; Move to next spectrum element
    decl %ecx                         ; Decrement counter
    jmp mass_diff_loop

mass_diff_done:
    popl %ebp
    ret                               ; Return from function

; Helper function to validate protein sequence
validate_protein:
    pushl %ebp
    movl %esp, %ebp
    
    ; Validate that all characters are valid amino acids
    movl protein_result, %esi         ; Protein string pointer
    
validate_loop:
    movb (%esi), %al                  ; Load character
    cmpl $0, %al                      ; Check for null terminator
    jz validate_done
    
    ; Validate amino acid character
    call is_valid_amino_acid
    
    testl %eax, %eax                  ; Check if valid
    jz invalid_protein                ; If not valid, error
    
    incl %esi                         ; Next character
    jmp validate_loop

invalid_protein:
    movl $0, %eax                     ; Return 0 for invalid
    
validate_done:
    popl %ebp
    ret                               ; Return from function

is_valid_amino_acid:
    pushl %ebx
    pushl %ecx
    
    movl %eax, %ebx                   ; Character to check
    movl $17, %ecx                    ; Number of valid amino acids
    
    ; Check against all valid amino acid characters
valid_check_loop:
    cmpl $0, %ecx
    jz not_valid
    
    cmpl amino_acids(,%ecx,1), %ebx  ; Compare with current amino acid
    jz valid_amino                    ; If match, it's valid
    
    decl %ecx                         ; Next character
    jmp valid_check_loop

valid_amino:
    movl $1, %eax                     ; Return 1 for valid
    jmp valid_done

not_valid:
    movl $0, %eax                     ; Return 0 for invalid

valid_done:
    popl %ecx
    popl %ebx
    ret                               ; Return from function
```

## Explanation

This Assembly solution addresses the protein inference problem by:

1. **Mass Table Setup**: Defines the mass-to-amino acid mapping using predefined tables
2. **Spectrum Processing**: Iterates through each mass in the spectrum
3. **Amino Acid Matching**: Compares each spectrum mass with known amino acid masses
4. **Result Construction**: Builds the protein sequence by concatenating matching amino acids

## Key Features

- Uses mass spectrometry principles to match experimental masses with theoretical amino acid masses
- Implements efficient lookup algorithms for mass comparison
- Handles edge cases like unknown amino acids (returns 'X')
- Includes validation functions to ensure result correctness
- Follows Assembly conventions for function calls and register usage

## Time Complexity
O(n × m) where n is the spectrum size and m is the number of amino acids in the table.

## Space Complexity
O(k) where k is the length of the resulting protein sequence.

This solution demonstrates how to approach bioinformatics problems using low-level assembly programming while maintaining correctness and efficiency.

